{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

module PD2 where

import PD2.Model
import PD2.Language
import PD2.Script

import Data.List.NonEmpty (NonEmpty, (<|), toList)
import System.Directory   (listDirectory, doesFileExist)
import Data.Foldable      (find)
import Data.Function      ((&))
import Data.Bool          (bool)

import qualified Data.Text      as T
import qualified System.Process as P

projectSetup :: Monad m => ConfigDir -> ProjectDir-> FileOps m -> LanguageBuildFileMapping -> m T.Text
projectSetup configDir projectDir fileOps lbMapping = do
  let repoPath = _projFolderPath projectDir
  maybeProjectScript <- hasScript configDir repoPath (_fileMatcher fileOps)
  case maybeProjectScript of
    Nothing       ->
      do
        maybeLang <- findLanguage projectDir lbMapping (_projectFileFinder fileOps)
        maybe (_defaultAction fileOps) (executeLanguageScript configDir fileOps) maybeLang
    (Just script) -> (_scriptRunner fileOps) (joinConfig configDir repoPath) script

