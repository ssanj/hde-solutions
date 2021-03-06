module PD2.Wiring where

import PD2.Model
import PD2.Script (executeLanguageScript, hasScript)
import PD2.Language (findLanguage)

defaultProjectOps :: Monad m => FileOps m -> ProcessOps m -> ProjectOps m
defaultProjectOps fo@(FileOps fileMatcher projectFileFinder) po@(ProcessOps scriptRunner defaultAction) =
  ProjectOps {
    _hasProjectScript      = hasScript fileMatcher
  , _findLanguage          = findLanguage projectFileFinder
  , _executeProjectScript  = \configDir repoPath   -> scriptRunner (joinConfig configDir repoPath) scriptFile
  , _executeLanguageScript = executeLanguageScript fo po
  , _executeDefaultScript  = defaultAction
  }