{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DerivingStrategies #-}

module PD1.Model where

import Data.List.NonEmpty (NonEmpty)

import qualified Data.Text as T


data Language = Scala
              | Ruby
              | Haskell deriving stock (Eq, Show)

newtype BuildFile = BuildFile { _buildFile :: File -> Bool }

newtype File = File { _file :: T.Text }

data LanguageBuildFiles = LanguageBuildFiles { _language :: Language, _buildFiles :: NonEmpty BuildFile }

type LanguageBuildFileMapping = NonEmpty LanguageBuildFiles

newtype ProjectDir = ProjectDir { _projectDir :: T.Text } deriving stock (Eq, Show)