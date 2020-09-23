{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

module PD2 where

import Data.List.NonEmpty (NonEmpty, (<|), toList)
import System.Directory   (listDirectory, doesFileExist)
import Data.Foldable      (find)
import Data.Function      ((&))
import Data.Bool          (bool)

import qualified Data.Text      as T
import qualified Data.Text.IO   as T
import qualified System.Process as P

data Language = Scala
              | Ruby
              | Haskell deriving stock (Eq, Show)

newtype BuildFile = BuildFile { _buildFile :: File -> Bool }

newtype File = File { _file :: T.Text }

newtype Directory = Directory { _dir :: T.Text }

data LanguageBuildFiles = LanguageBuildFiles { _language :: Language, _buildFiles :: NonEmpty BuildFile }

type LanguageBuildFileMapping = NonEmpty LanguageBuildFiles

newtype ProjectDir = ProjectDir { _projectDir :: T.Text } deriving stock (Eq, Show)

newtype ConfigDir = ConfigDir { _configDir :: T.Text } deriving stock (Eq, Show)

newtype RepoPath = RepoPath { _repoPath :: NonEmpty T.Text } deriving stock (Eq, Show)

data FileOps m =
  FileOps {
    _fileMatcher :: File -> m Bool
  , _projectFileFinder :: ProjectDir -> m [File]
  , _scriptRunner :: Directory -> File -> m T.Text
  , _defaultAction :: m T.Text
}

joinConfig :: ConfigDir -> RepoPath -> Directory
joinConfig (ConfigDir dir) (RepoPath paths) = Directory $ T.intercalate "/" (toList $ dir <| paths)

joinProject :: ProjectDir -> RepoPath -> Directory
joinProject (ProjectDir dir) (RepoPath paths) = Directory $ T.intercalate "/" (toList $ dir <| paths)

scriptFile :: File
scriptFile = File "script.sh"

appendPath :: Directory -> File -> File
appendPath (Directory dir) (File file) = File $ T.intercalate "/" [dir, file]

hasScript :: Monad m => ConfigDir -> RepoPath -> (File -> m Bool) -> m (Maybe File)
hasScript configDir repoPath fileFinder =
  let directory  = joinConfig configDir repoPath
      scriptPath = appendPath directory scriptFile
  in do
    found <- fileFinder scriptPath
    pure $ bool Nothing (Just scriptPath) found

executeScript :: (File -> m T.Text) -> File -> m T.Text
executeScript exector = exector

projectSetup :: Monad m => ConfigDir -> ProjectDir -> RepoPath -> FileOps m -> LanguageBuildFileMapping -> m T.Text
projectSetup configDir projectDir repoPath fileOps lbMapping = do
  maybeProjectScript <- hasScript configDir repoPath (_fileMatcher fileOps)
  case maybeProjectScript of
    Nothing       ->
      do
        maybeLang <- findLanguage projectDir lbMapping (_projectFileFinder fileOps)
        maybe (_defaultAction fileOps) (executeLanguageScript configDir fileOps) maybeLang
    (Just script) -> (_scriptRunner fileOps) (joinConfig configDir repoPath) script

executeLanguageScript :: Monad m => ConfigDir -> FileOps m -> Language -> m T.Text
executeLanguageScript configDir fileOps lang = do
  let repoPath = RepoPath $ pure (T.pack . show $ lang)
  maybeScript <- hasScript configDir repoPath (_fileMatcher fileOps)
  maybe (_defaultAction fileOps) (_scriptRunner fileOps (joinConfig configDir repoPath)) maybeScript

scalaMapping :: LanguageBuildFiles
scalaMapping  =
  let buildFile = BuildFile (("build.sbt" ==) . _file)
  in LanguageBuildFiles Scala (pure buildFile)

rubyMapping :: LanguageBuildFiles
rubyMapping  =
  let buildFile = BuildFile (("Gemfile" ==) . _file)
  in LanguageBuildFiles Ruby (pure buildFile)

haskellMapping :: LanguageBuildFiles
haskellMapping  =
  let matches :: File -> Bool
      matches (File file) = file == "stack.yaml" || (".cabal") `T.isSuffixOf` file
      buildFile = BuildFile matches
  in LanguageBuildFiles Haskell (pure buildFile)

languageMappings :: LanguageBuildFileMapping
languageMappings = scalaMapping <| rubyMapping <| (pure haskellMapping)

fileOpsIO :: FileOps IO
fileOpsIO =
  FileOps {
    _fileMatcher       = fileMatcherIO
  , _projectFileFinder = findProjectFilesInDirIO
  , _scriptRunner      = scriptRunnerIO
  , _defaultAction     = defaultActionIO
  }

fileMatcherIO :: File -> IO Bool
fileMatcherIO = doesFileExist . T.unpack . _file

defaultActionIO :: IO T.Text
defaultActionIO = pure "no setup needed"

scriptRunnerIO :: Directory -> File -> IO T.Text
scriptRunnerIO (Directory wd) (File script) =
  fmap T.pack $
    P.readCreateProcess
      (P.shell (T.unpack script)){
        P.cwd = Just . T.unpack $ wd
      }
      ""

findProjectFilesInDirIO :: ProjectDir -> IO [File]
findProjectFilesInDirIO (ProjectDir projectDir) =
  let filesList = listDirectory (T.unpack projectDir)
  in fmap (fmap (File . T.pack)) filesList

findLanguage :: Monad m => ProjectDir -> LanguageBuildFileMapping -> (ProjectDir -> m [File]) -> m (Maybe Language)
findLanguage projDir lbFiles fileFinder = do
  files <- fileFinder projDir
  let maybeFound = find (hasLanguageBuildFile files) lbFiles
  pure $ fmap _language maybeFound

hasLanguageBuildFile :: [File] -> LanguageBuildFiles -> Bool
hasLanguageBuildFile [] _ = False
hasLanguageBuildFile (x:xs) lb@(LanguageBuildFiles l bf) =
  let maybeFound = find ((x &) . _buildFile) bf
  in maybe (hasLanguageBuildFile xs lb) (const True) maybeFound
