{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

module PD1 where

import Data.List.NonEmpty (NonEmpty, (<|))
import System.Directory   (listDirectory)
import Data.Foldable      (find)

import qualified Data.Text    as T
import qualified Data.Text.IO as T

data Language = Scala
              | Ruby
              | Haskell deriving stock (Eq, Show)

newtype BuildFile = BuildFile { _buildFile :: File -> Bool }

newtype File = File { _file :: T.Text }

data LanguageBuildFiles = LanguageBuildFiles { _language :: Language, _buildFiles :: NonEmpty BuildFile }

type LanguageBuildFileMapping = NonEmpty LanguageBuildFiles

newtype ProjectDir = ProjectDir { _projectDir :: T.Text } deriving stock (Eq, Show)

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

findFilesInDirIO :: ProjectDir -> IO [File]
findFilesInDirIO (ProjectDir projectDir) =
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
  let maybeFound = find (\(BuildFile f) -> f x) bf
  in maybe (hasLanguageBuildFile xs lb) (const True) maybeFound
