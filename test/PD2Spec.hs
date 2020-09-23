{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PD2Spec where

import PD2
import Data.Functor.Identity
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
      found              = True
      output             = "some output"
      resultId           = projectSetup configDir projectDir (fileOpsFindAndScript found output) languageBuildFiles
      expected           = output
      actual = runIdentity resultId
  in actual @?= expected


fileOpsFindAndScript :: Bool -> T.Text -> FileOps Identity
fileOpsFindAndScript bool output =
  defaultFileOps {
    _fileMatcher = const $ pure bool
  , _scriptRunner = \_ _ -> pure output
  }


defaultFileOps :: FileOps Identity
defaultFileOps =
  FileOps {
    _fileMatcher       = undefined
  , _projectFileFinder = undefined
  , _scriptRunner      = undefined
  , _defaultAction     = undefined
  }

  -- FileOps {
  --   _fileMatcher :: File -> m Bool
  -- , _projectFileFinder :: ProjectDir -> m [File]
  -- , _scriptRunner :: Directory -> File -> m T.Text
  -- , _defaultAction :: m T.Text
  -- }

-- unit_no_matching_languages :: Assertion
-- unit_no_matching_languages =
--   let projectDir    = ProjectDir "whatever"
--       mappings      = pure scalaMapping
--       filesProvider = (\_ -> Identity $ [File "blee", File "blah"])
--       resultId      = findLanguage projectDir mappings filesProvider
--       actual        = runIdentity resultId
--       expected      = Nothing
--   in  actual @?= expected
