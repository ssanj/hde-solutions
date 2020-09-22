{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PD1Spec where

import Data.Functor.Identity
import Prelude hiding (reverse)

import Test.Tasty.HUnit   ((@?=), Assertion)
import Data.List.NonEmpty (reverse)
import PD1

unit_no_matching_languages :: Assertion
unit_no_matching_languages =
  let projectDir    = ProjectDir "whatever"
      mappings      = pure scalaMapping
      filesProvider = (\_ -> Identity $ [File "blee", File "blah"])
      resultId      = findLanguage projectDir mappings filesProvider
      actual        = runIdentity resultId
      expected      = Nothing
  in  actual @?= expected

unit_no_files_returned :: Assertion
unit_no_files_returned =
  let projectDir    = ProjectDir "whatever"
      mappings      = languageMappings
      filesProvider = (\_ -> Identity $ [])
      resultId      = findLanguage projectDir mappings filesProvider
      actual        = runIdentity resultId
      expected      = Nothing
  in  actual @?= expected

unit_matching_scala :: Assertion
unit_matching_scala =
  let projectDir    = ProjectDir "whatever"
      mappings      = pure scalaMapping
      filesProvider = (\_ -> Identity $ [File "blee", File "blah", File "build.sbt"])
      resultId      = findLanguage projectDir mappings filesProvider
      actual        = runIdentity resultId
      expected      = Just Scala
  in  actual @?= expected

unit_returns_first_match :: Assertion
unit_returns_first_match =
  let projectDir    = ProjectDir "whatever"
      mappings      = reverse languageMappings -- Haskell is checked first
      filesProvider = (\_ -> Identity $ [File "build.sbt", File "Gemfile", File "blee.cabal"])
      resultId      = findLanguage projectDir mappings filesProvider
      actual        = runIdentity resultId
      expected      = Just Haskell
  in  actual @?= expected

unit_matches_haskell_by_cabal :: Assertion
unit_matches_haskell_by_cabal =
  let projectDir    = ProjectDir "whatever"
      mappings      = pure haskellMapping
      filesProvider = (\_ -> Identity $ [File "blee", File "blah", File "blee.cabal", File "boo"])
      resultId      = findLanguage projectDir mappings filesProvider
      actual        = runIdentity resultId
      expected      = Just Haskell
  in  actual @?= expected

unit_matches_haskell_by_stack :: Assertion
unit_matches_haskell_by_stack =
  let projectDir    = ProjectDir "whatever"
      mappings      = pure haskellMapping
      filesProvider = (\_ -> Identity $ [File "blee", File "blah", File "boo", File "stack.yaml"])
      resultId      = findLanguage projectDir mappings filesProvider
      actual        = runIdentity resultId
      expected      = Just Haskell
  in  actual @?= expected
