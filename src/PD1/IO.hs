{-# LANGUAGE OverloadedStrings #-}

module PD1.IO where

import PD1.Model

import System.Directory   (listDirectory)

import qualified Data.Text    as T

findFilesInDirIO :: ProjectDir -> IO [File]
findFilesInDirIO (ProjectDir projectDir) =
  let filesList = listDirectory (T.unpack projectDir)
  in fmap (fmap (File . T.pack)) filesList