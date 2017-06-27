{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Plugin (Plugin,
               runPlugin,
               envPlugin,
               convert) where

import           Control.Monad.Except
import           Control.Monad.Trans  (MonadIO)
import qualified Data.Text            as T
import           System.Environment   (lookupEnv)


newtype Plugin m = Plugin
  { runPlugin :: String -> String -> ExceptT String m (Maybe T.Text) }

envPlugin :: MonadIO m => Plugin m
envPlugin = makePlugin "env" $ \k -> do
  s <- liftIO $ lookupEnv k
  return (s >>= convert)

makePlugin :: Monad m => String -> (String -> m (Maybe T.Text)) -> Plugin m
makePlugin match f = Plugin $ \name arg ->
  if name == match
  then ExceptT $ Right <$> f arg
  else runPlugin failPlugin name arg

failPlugin :: Monad m => Plugin m
failPlugin = Plugin $ \name _ ->
  throwError $ "function '" ++ name ++ "' not defined"

convert :: String -> Maybe T.Text
convert = Just . T.pack

instance Monad m => Monoid (Plugin m) where
  mempty = failPlugin
  mappend = undefined


