{-# LANGUAGE OverloadedStrings #-}

module PD2.IO where

import PD2.Model

import System.Directory   (listDirectory, doesFileExist)

import qualified Data.Text      as T
import qualified System.Process as P

fileOpsIO :: FileOps IO
fileOpsIO =
  FileOps {
    _fileMatcher       = fileMatcherIO
  , _projectFileFinder = findProjectFilesInDirIO
  }

processOpsIO :: ProcessOps IO
processOpsIO =
  ProcessOps {
    _scriptRunner  = scriptRunnerIO
  , _defaultAction = defaultActionIO
  }

fileMatcherIO :: File -> IO Bool
fileMatcherIO = doesFileExist . T.unpack . _file

defaultActionIO :: ConfigDir -> IO T.Text
defaultActionIO = pure . const "no setup needed"

scriptRunnerIO :: Directory -> File -> IO T.Text
scriptRunnerIO (Directory wd) (File script) =
  fmap T.pack $
    P.readCreateProcess
      (P.shell (T.unpack script)){
        P.cwd = Just . T.unpack $ wd
      }
      ""

findProjectFilesInDirIO :: ProjectDir -> IO [File]
findProjectFilesInDirIO  projectDir =
  let filesList = listDirectory (T.unpack . _dir . joinProject $ projectDir)
  in fmap (fmap (File . T.pack)) filesList