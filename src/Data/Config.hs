module Data.Config
  ( module Data.Config.Eval
  , MonadEnv(..)
  , defaultPlugin
  , decodeWithDefault
  , envPlugin
  , idPlugin
  , makePlugin
  ) where

import           Control.Monad.Catch (MonadThrow)
import           Data.Aeson          (FromJSON)
import qualified Data.ByteString     as B

import           Data.Config.Eval
import           Data.Config.Plugin  (MonadEnv (..), Plugin, envPlugin,
                                      idPlugin, makePlugin, (<>))



decodeWithDefault :: (MonadEnv m, MonadThrow m, FromJSON a)
                        => B.ByteString -> m a
decodeWithDefault = decodeWithPlugin defaultPlugin

defaultPlugin :: MonadEnv m => Plugin m
defaultPlugin = envPlugin <> idPlugin

