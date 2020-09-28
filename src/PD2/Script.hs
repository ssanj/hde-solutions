module PD2.Script where

import PD2.Model

import Data.Bool (bool)

import qualified Data.Text as T

hasScript :: Monad m => ConfigDir -> RepoPath -> (File -> m Bool) -> m (Maybe File)
hasScript configDir repoPath fileFinder =
  let directory  = joinConfig configDir repoPath
      scriptPath = appendPath directory scriptFile
  in do
    found <- fileFinder scriptPath -- TODO: Is this going to match on abs location or file name?
    pure $ bool Nothing (Just scriptPath) found

executeLanguageScript :: Monad m => FileOps m -> ProcessOps m -> ConfigDir -> Language -> m T.Text
executeLanguageScript (FileOps fileMatcher _) (ProcessOps scriptRunner defaultAction) configDir lang = do
  let repoPath = RepoPath $ pure (T.pack . show $ lang)
  maybeScript <- hasScript configDir repoPath fileMatcher
  maybe (defaultAction configDir) (\_ -> scriptRunner (joinConfig configDir repoPath) scriptFile) maybeScript

