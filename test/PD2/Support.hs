module PD2.Support where

import PD2.Model
import Data.Functor.Identity (Identity)
import qualified Data.Text    as T


testProjectOps ::
  (ConfigDir  -> RepoPath                 -> m (Maybe File))     ->
  (ProjectDir -> LanguageBuildFileMapping -> m (Maybe Language)) ->
  (ConfigDir  -> RepoPath                 -> m T.Text)           ->
  (ConfigDir  -> Language                 -> m T.Text)           ->
  (ConfigDir                              -> m T.Text)           ->
  ProjectOps m
testProjectOps
  hasProjectScript
  findLanguage
  executeProjectScript
  executeLanguageScript
  executeDefaultScript =
    ProjectOps {
      _hasProjectScript      = hasProjectScript
    , _findLanguage          = findLanguage
    , _executeProjectScript  = executeProjectScript
    , _executeLanguageScript = executeLanguageScript
    , _executeDefaultScript  = executeDefaultScript
    }

withProjectScript :: File -> T.Text -> ProjectOps Identity
withProjectScript script output =
  defaultProjectOps {
    _hasProjectScript     = \_ _ -> pure . pure $ script
  , _executeProjectScript = \_ _ -> pure output
  }

withoutProjectScriptWithLang :: Language -> T.Text -> ProjectOps Identity
withoutProjectScriptWithLang lang output =
  defaultProjectOps {
    _hasProjectScript      = \_ _ -> pure Nothing
  , _findLanguage          = \_ _ -> pure . pure $ lang
  , _executeLanguageScript = \_ _ -> pure output
  }

withNoMatches :: T.Text -> ProjectOps Identity
withNoMatches output =
  defaultProjectOps {
    _hasProjectScript      = \_ _ -> pure Nothing
  , _findLanguage          = \_ _ -> pure Nothing
  , _executeDefaultScript  = \_   -> pure output
  }

defaultProjectOps :: ProjectOps Identity
defaultProjectOps =
  ProjectOps {
    _hasProjectScript      = undefined
  , _findLanguage          = undefined
  , _executeProjectScript  = undefined
  , _executeLanguageScript = undefined
  , _executeDefaultScript  = undefined
  }