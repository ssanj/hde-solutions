{-# LANGUAGE OverloadedStrings #-}

module PD2.Language where

import PD2.Model

import Data.List.NonEmpty ((<|))
import Data.Foldable      (find)
import Data.Function      ((&))

import qualified Data.Text      as T

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

findLanguage :: Monad m => (ProjectDir -> m [File]) -> ProjectDir -> LanguageBuildFileMapping ->  m (Maybe Language)
findLanguage fileFinder projDir lbFiles = do
  files <- fileFinder projDir
  let maybeFound = find (hasLanguageBuildFile files) lbFiles
  pure $ fmap _language maybeFound

hasLanguageBuildFile :: [File] -> LanguageBuildFiles -> Bool
hasLanguageBuildFile [] _ = False
hasLanguageBuildFile (x:xs) lb@(LanguageBuildFiles l bf) =
  let maybeFound = find ((x &) . _buildFile) bf
  in maybe (hasLanguageBuildFile xs lb) (const True) maybeFound
