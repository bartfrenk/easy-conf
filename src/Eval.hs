module Eval (evalText,
             eval,
             evalValue,
             decodeWithEval) where

import           Control.Monad.Except
import           Control.Monad.Trans  (MonadIO)
import           Data.Aeson           hiding (decode)
import qualified Data.ByteString      as B
import           Data.Maybe           (fromMaybe)
import           Data.String.Conv     (toS)
import qualified Data.Text            as T
import           Data.Yaml
import           System.Environment

import           Parser

evalText :: MonadIO m => T.Text -> ExceptT String m T.Text
evalText s = do
  v' <- parseExpr s >>= eval
  case v' of
    Nothing ->
      throwError $ "valid expression '" ++ toS s ++ "' did not return a value"
    Just v -> return v

eval :: MonadIO m => Expr -> m (Maybe T.Text)
eval (TmGetEnv k) = do s <- liftIO $ lookupEnv k; return (s >>= convert)
eval (TmLit s) = return $ convert s
eval (TmOr e f) = do
  v' <- eval e
  case v' of
    Just v  -> return $ Just v
    Nothing -> eval f

convert :: String -> Maybe T.Text
convert = Just . T.pack

evalValue :: MonadIO m => Value -> ExceptT String m Value
evalValue (String t) = redecode <$> evalText t
  where redecode t0 = fromMaybe (String t0) (decode $ toS t0)
evalValue (Object obj) = Object <$> evalValue `traverse` obj
evalValue (Array arr) = Array <$> evalValue `traverse` arr
evalValue v = return v

decodeM :: (Monad m, FromJSON a) => B.ByteString -> ExceptT String m a
decodeM bs = case decodeEither bs of
  Right val -> return val
  Left err  -> throwError err

decodeWithEval :: FromJSON a => B.ByteString -> IO (Either String a)
decodeWithEval bs = runExceptT $ do
  value <- decodeM bs
  res <- fromJSON <$> evalValue value
  case res of
    Error msg -> throwError msg
    Success a -> return a
