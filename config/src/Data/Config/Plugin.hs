{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Config.Plugin
  ( Plugin
  , runPlugin
  , envPlugin
  , failPlugin
  , idPlugin
  , convert
  , (<>)
  ) where

import Control.Monad.Except
import Control.Monad.Trans (MonadIO)
import Data.Monoid ((<>))
import qualified Data.Text as T
import System.Environment (lookupEnv)

newtype Plugin m = Plugin
  { runPlugin :: String -> String -> ExceptT String m (Maybe T.Text)
  }

envPlugin :: MonadIO m => Plugin m
envPlugin =
  makePlugin "env" $ \arg -> do
    s <- liftIO $ lookupEnv arg
    return (s >>= convert)

idPlugin :: Monad m => Plugin m
idPlugin = makePlugin "id" $ \arg -> return $ convert arg

makePlugin :: Monad m => String -> (String -> m (Maybe T.Text)) -> Plugin m
makePlugin match f =
  Plugin $ \name arg ->
    if name == match
      then ExceptT $ Right <$> f arg
      else runPlugin failPlugin name arg

failPlugin :: Monad m => Plugin m
failPlugin =
  Plugin $ \name _ -> throwError $ "function '" ++ name ++ "' not defined"

convert :: String -> Maybe T.Text
convert = Just . T.pack

instance Monad m => Monoid (Plugin m) where
  mempty = failPlugin
  p `mappend` q =
    Plugin $ \name arg ->
      ExceptT $ do
        res' <- runExceptT (runPlugin p name arg)
        case res' of
          Left _ -> runExceptT $ runPlugin q name arg
          Right res -> return $ Right res
