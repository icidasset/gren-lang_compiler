{-# LANGUAGE OverloadedStrings #-}

module Run
  ( Flags (..),
    run
  )
where

import BackgroundWriter qualified as BW
import Data.ByteString.Builder qualified as B
import Data.NonEmptyList qualified as NE
import Directories qualified as Dirs
import Generate.JavaScript qualified as JS
import Gren.Details qualified as Details
import Gren.Platform qualified as Platform
import Make qualified
import Parse.Module qualified as Parse
import Reporting qualified
import Reporting.Exit qualified as Exit
import Reporting.Task qualified as Task
import System.IO qualified as IO
import System.Process qualified as Process

-- FLAGS

data Flags = Flags
  { _optimize :: Bool,
    _sourceMaps :: Bool,
    _report :: Maybe Make.ReportType
  }

-- RUN

run :: [FilePath] -> Flags -> IO ()
run paths flags@(Flags _ _ report) =
  do
    style <- Make.getStyle Nothing report
    maybeRoot <- Dirs.findRoot

    (Just hin, _, _, nodeProcess) <- Process.createProcess
      (Process.shell "echo")
      { Process.std_in = Process.CreatePipe
      , Process.std_out = Process.UseHandle IO.stdout
      }

    Reporting.attemptWithStyle style Exit.makeToReport $
      case maybeRoot of
        Just root -> runHelp root paths style flags hin
        Nothing -> return $ Left Exit.MakeNoOutline

    IO.hClose hin

    _ <- Process.waitForProcess nodeProcess

    return ()


runHelp :: FilePath -> [FilePath] -> Reporting.Style -> Flags -> IO.Handle -> IO (Either Exit.Make ())
runHelp root paths style (Flags optimize withSourceMaps _) handle =
  BW.withScope $ \scope ->
    Dirs.withRootLock root $
      Task.run $
        do
          desiredMode <- Make.getMode False optimize
          details <- Task.eio Exit.MakeBadDetails (Details.load style scope root)
          let platform = Make.getPlatform details
          let projectType = Make.getProjectType details
          case projectType of
            Parse.Package _ ->
              Task.throw Exit.MakeCannotOutputForPackage
            _ ->
              case paths of
                [] ->
                  -- TODO: Custom error for `run`
                  do
                    exposed <- Make.getExposed details
                    Make.buildExposed style root details exposed
                p : ps ->
                  do
                    artifacts <- Make.buildPaths style root details (NE.List p ps)
                    let mains = Make.getMains artifacts
                    case (platform, mains) of
                      (Platform.Node, _) ->
                        do
                          (JS.GeneratedResult source sourceMap) <- Make.generate root details desiredMode artifacts
                          bundle <- Make.prepareOutput withSourceMaps root 0 sourceMap source
                          Task.io $ B.hPutBuilder handle bundle
                      _ ->
                        return ()
