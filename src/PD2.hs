{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

module PD2 (
   module PD2.Model
,  module PD2.Language
,  projectSetup
)where

import PD2.Model
import PD2.Language

import Data.List.NonEmpty (NonEmpty, (<|), toList)
import System.Directory   (listDirectory, doesFileExist)
import Data.Foldable      (find)
import Data.Function      ((&))
import Data.Bool          (bool)

import qualified Data.Text      as T
import qualified System.Process as P

projectSetup :: Monad m => ProjectOps m -> ConfigDir -> ProjectDir -> LanguageBuildFileMapping -> m T.Text
projectSetup (ProjectOps hasProjectScript findLanguage xProjectScript xLanguageScript xDefaultScript) configDir projectDir lbMapping = do
  let repoPath = _projFolderPath projectDir
  maybeProjectScript <- hasProjectScript configDir repoPath
  case maybeProjectScript of
    Nothing       ->
      do
        maybeLang <- findLanguage projectDir lbMapping
        maybe (xDefaultScript configDir) (xLanguageScript configDir) maybeLang
    (Just _) -> xProjectScript configDir repoPath
