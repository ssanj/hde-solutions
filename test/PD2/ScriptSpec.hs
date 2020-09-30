{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PD2.ScriptSpec where

import PD2.Model
import PD2.Script
import PD2.Support

import Data.List.NonEmpty    ((<|))
import Data.Functor.Identity (Identity(..), runIdentity)
import Test.Tasty.HUnit      (assertFailure, (@?=), Assertion)

unit_hasScript_script_found :: Assertion
unit_hasScript_script_found =
  let fileMatcher = (pure . const True)
      configDir   = ConfigDir "some_config_dir"
      repoPath    = RepoPath ("one" <| "two" <| pure "three")
      output      = File "some_config_dir/one/two/three/script.sh"
      expected    = Identity . Just $ output
      actual      = hasScript fileMatcher configDir repoPath
  in actual @?= expected

unit_hasScript_script_not_found :: Assertion
unit_hasScript_script_not_found =
  let fileMatcher = (pure . const False)
      configDir   = ConfigDir "some_config_dir"
      repoPath    = RepoPath ("one" <| "two" <| pure "three")
      output      = File "some_config_dir/one/two/three/script.sh"
      expected    = Identity Nothing
      actual      = hasScript fileMatcher configDir repoPath
  in actual @?= expected

unit_executeLanguageScript_has_script :: Assertion
unit_executeLanguageScript_has_script =
  let
      configDir  = ConfigDir "some_config_dir"
      language   = Ruby
      output     = "some dynamic output"
      fileOps    = withFileMatcher True
      processOps = withProcessOps output undefined
      expected   = Identity output
      actual     = executeLanguageScript fileOps processOps configDir language
  in actual @?= expected

unit_executeLanguageScript_no_script :: Assertion
unit_executeLanguageScript_no_script =
  let
      configDir  = ConfigDir "some_config_dir"
      language   = Ruby
      output     = "some default output"
      fileOps    = withFileMatcher False
      processOps = withProcessOps undefined output
      expected   = Identity output
      actual     = executeLanguageScript fileOps processOps configDir language
  in actual @?= expected