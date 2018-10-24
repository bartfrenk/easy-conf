module Data.Config
  ( module Data.Config.Eval
  , defaultPlugin
  , envPlugin
  , idPlugin
  , makePlugin
  ) where

import           Control.Monad.Trans (MonadIO)

import           Data.Config.Eval
import           Data.Config.Plugin  (Plugin, envPlugin, idPlugin, makePlugin,
                                      (<>))

defaultPlugin :: MonadIO m => Plugin m
defaultPlugin = envPlugin <> idPlugin

