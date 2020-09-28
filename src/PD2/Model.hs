{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

module PD2.Model where

import Data.List.NonEmpty (NonEmpty, (<|), toList)

import qualified Data.Text      as T

data Language = Scala
              | Ruby
              | Haskell deriving stock (Eq, Show)

newtype BuildFile = BuildFile { _buildFile :: File -> Bool }

newtype File = File { _file :: T.Text }

newtype Directory = Directory { _dir :: T.Text }

data LanguageBuildFiles = LanguageBuildFiles { _language :: Language, _buildFiles :: NonEmpty BuildFile }

type LanguageBuildFileMapping = NonEmpty LanguageBuildFiles

newtype CheckoutDir = CheckoutDir { _checkoutDir :: T.Text } deriving stock (Eq, Show)

data ProjectDir = ProjectDir { _projCheckoutDir :: CheckoutDir, _projFolderPath :: RepoPath } deriving stock (Eq, Show)

newtype ConfigDir = ConfigDir { _configDir :: T.Text } deriving stock (Eq, Show)

newtype RepoPath = RepoPath { _repoPath :: NonEmpty T.Text } deriving stock (Eq, Show)

data FileOps m =
  FileOps {
    _fileMatcher :: File -> m Bool
  , _projectFileFinder :: ProjectDir -> m [File]
}

data ProcessOps m =
  ProcessOps {
    _scriptRunner  :: Directory -> File -> m T.Text
  , _defaultAction :: ConfigDir         -> m T.Text
  }

data ProjectOps m =
  ProjectOps {
    _hasProjectScript      :: ConfigDir  -> RepoPath                 -> m (Maybe File)
  , _findLanguage          :: ProjectDir -> LanguageBuildFileMapping -> m (Maybe Language)
  , _executeProjectScript  :: ConfigDir  -> RepoPath                 -> m T.Text
  , _executeLanguageScript :: ConfigDir  -> Language                 -> m T.Text
  , _executeDefaultScript  :: ConfigDir                              -> m T.Text
  }

joinConfig :: ConfigDir -> RepoPath -> Directory
joinConfig (ConfigDir dir) (RepoPath paths) = Directory $ T.intercalate "/" (toList $ dir <| paths)

joinProject :: ProjectDir -> Directory
joinProject (ProjectDir (CheckoutDir dir) (RepoPath paths)) = Directory $ T.intercalate "/" (toList $ dir <| paths)

scriptFile :: File
scriptFile = File "script.sh"

appendPath :: Directory -> File -> File
appendPath (Directory dir) (File file) = File $ T.intercalate "/" [dir, file]
