module PD2.Script where

import PD2.Model

import Data.Bool (bool)

import qualified Data.Text as T

hasScript :: Monad m => ConfigDir -> RepoPath -> (File -> m Bool) -> m (Maybe File)
hasScript configDir repoPath fileFinder =
  let directory  = joinConfig configDir repoPath
      scriptPath = appendPath directory scriptFile
  in do
    found <- fileFinder scriptPath
    pure $ bool Nothing (Just scriptPath) found

executeLanguageScript :: Monad m => ConfigDir -> FileOps m -> Language -> m T.Text
executeLanguageScript configDir fileOps lang = do
  let repoPath = RepoPath $ pure (T.pack . show $ lang)
  maybeScript <- hasScript configDir repoPath (_fileMatcher fileOps)
  maybe (_defaultAction fileOps) (_scriptRunner fileOps (joinConfig configDir repoPath)) maybeScript

