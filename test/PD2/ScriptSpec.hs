{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PD2.ScriptSpec where

import PD2.Model
import PD2.Script

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


