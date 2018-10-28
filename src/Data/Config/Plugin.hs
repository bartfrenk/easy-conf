{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Config.Plugin
  ( Plugin
  , PluginResult(..)
  , MonadEnv(..)
  , envPlugin
  , idPlugin
  , makePlugin
  , throwRunPlugin
  , (<>)
  ) where

import           Control.Monad.Catch (Exception, MonadThrow, throwM)
import           Control.Monad.Trans (MonadIO(..))
import           Data.Monoid         ((<>))
import           Data.String         (IsString (..))
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Typeable       (Typeable)
import qualified System.Environment  as Sys


type PluginMatch = String

data PluginResult =
  NoMatch | NoResult | Result Text

data PluginException
  = NoMatchingPlugin PluginMatch
  deriving (Show, Typeable)

instance Exception PluginException

instance IsString PluginResult where
  fromString s = Result $ T.pack s

newtype Plugin m = Plugin
  { runPlugin :: PluginMatch -> String -> m PluginResult
  }

class Monad m => MonadEnv m where
  lookupEnv :: String -> m (Maybe String)

instance (Monad m, MonadIO m) => MonadEnv m where
  lookupEnv = liftIO . Sys.lookupEnv

envPlugin :: MonadEnv m => Plugin m
envPlugin =
  makePlugin "env" $ \arg ->
    lookupEnv arg >>= \case
      Nothing -> pure NoResult
      Just val -> pure $ fromString val

idPlugin :: Monad m => Plugin m
idPlugin = makePlugin "id" $ \arg -> pure $ fromString arg

makePlugin :: Monad m => PluginMatch -> (String -> m PluginResult) -> Plugin m
makePlugin match f =
  Plugin $ \name arg ->
    if name == match
      then f arg
      else pure $ NoMatch

neverMatch :: Monad m => Plugin m
neverMatch = Plugin $ \_ _ -> pure NoMatch

instance Monad m => Monoid (Plugin m) where
  mempty = neverMatch
  p `mappend` q = Plugin $ \name arg -> do
    runPlugin p name arg >>= \case
      NoMatch -> runPlugin q name arg
      result@_ -> pure result


throwRunPlugin :: MonadThrow m => Plugin m -> PluginMatch -> String -> m (Maybe Text)
throwRunPlugin plugin match arg = runPlugin plugin match arg >>= \case
  NoMatch -> throwM $ NoMatchingPlugin match
  NoResult -> pure Nothing
  Result txt -> pure $ Just txt
