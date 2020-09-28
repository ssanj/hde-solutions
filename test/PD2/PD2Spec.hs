{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PD2.PD2Spec where

import PD2
import PD2.Support
import Data.Functor.Identity (runIdentity)
import Prelude hiding (reverse)

import Test.Tasty.HUnit   ((@?=), Assertion)
import Data.List.NonEmpty ((<|))

import qualified Data.Text as T

unit_has_project_script :: Assertion
unit_has_project_script =
  let configDir          = ConfigDir "someConfig"
      checkoutDir        = CheckoutDir "someCheckout"
      repoPath           = RepoPath ("some" <| "repo" <| (pure "path"))
      projectDir         = ProjectDir checkoutDir repoPath
      languageBuildFiles = undefined
      projectScript      = File "somefile"
      output             = "some output"
      projectOps         = withProjectScript projectScript output
      resultId           = projectSetup projectOps configDir projectDir  languageBuildFiles
      expected           = output
      actual = runIdentity resultId
  in actual @?= expected

unit_no_project_script_matching_lang :: Assertion
unit_no_project_script_matching_lang =
  let configDir          = ConfigDir "someConfig"
      checkoutDir        = CheckoutDir "someCheckout"
      repoPath           = RepoPath ("some" <| "repo" <| (pure "path"))
      projectDir         = ProjectDir checkoutDir repoPath
      languageBuildFiles = undefined
      projectScript      = File "somefile"
      output             = "some scala script output"
      projectOps         = withoutProjectScriptWithLang Scala output
      resultId           = projectSetup projectOps configDir projectDir languageBuildFiles
      expected           = output
      actual = runIdentity resultId
  in actual @?= expected

unit_no_project_script_no_matching_lang :: Assertion
unit_no_project_script_no_matching_lang =
  let configDir          = ConfigDir "someConfig"
      checkoutDir        = CheckoutDir "someCheckout"
      repoPath           = RepoPath ("some" <| "repo" <| (pure "path"))
      projectDir         = ProjectDir checkoutDir repoPath
      languageBuildFiles = undefined
      projectScript      = File "somefile"
      output             = "no setup needed"
      projectOps         = withNoMatches output
      resultId           = projectSetup projectOps configDir projectDir languageBuildFiles
      expected           = output
      actual = runIdentity resultId
  in actual @?= expected
