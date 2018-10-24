module Data.Config
  ( module Data.Config.Eval
  , MonadEnv(..)
  , defaultPlugin
  , decodeWithDefault
  , envPlugin
  , idPlugin
  , makePlugin
  , readValue
  , extractConfig
  ) where

import           Control.Monad.Catch (MonadThrow)
import           Control.Monad.Trans (MonadIO, liftIO)
import           Data.Aeson          (FromJSON, Value)
import qualified Data.ByteString     as B
import           Data.Text           (Text)

import           Data.Config.Eval
import           Data.Config.Plugin  (MonadEnv (..), Plugin, envPlugin,
                                      idPlugin, makePlugin, (<>))



decodeWithDefault :: (MonadEnv m, MonadThrow m, FromJSON a)
                        => B.ByteString -> m a
decodeWithDefault = decodeWithPlugin defaultPlugin

defaultPlugin :: MonadEnv m => Plugin m
defaultPlugin = envPlugin <> idPlugin

readValue :: (MonadIO m, MonadThrow m) => FilePath -> m Value
readValue path = liftIO $ B.readFile path >>= throwDecode

extractConfig :: (MonadEnv m, MonadThrow m) => FromJSON a => Text -> Value -> m a
extractConfig key = decodeWithPluginNested defaultPlugin [key]


