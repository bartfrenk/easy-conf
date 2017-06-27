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

import           Parser
import           Plugin

evalText :: MonadIO m => Plugin m -> T.Text -> ExceptT String m T.Text
evalText p s = do
  v' <- parseExpr s >>= eval p
  case v' of
    Nothing ->
      throwError $ "valid expression '" ++ toS s ++ "' did not return a value"
    Just v -> return v

eval :: MonadIO m => Plugin m -> Expr -> ExceptT String m (Maybe T.Text)
eval p (TmFunc name arg) = runPlugin p name arg
eval _ (TmLit s) = return $ convert s
eval p (TmOr e f) = do
  v' <- eval p e
  case v' of
    Just v  -> return $ Just v
    Nothing -> eval p f


evalValue :: MonadIO m => Plugin m -> Value -> ExceptT String m Value
evalValue p (String t) = redecode <$> evalText p t
  where redecode t0 = fromMaybe (String t0) (decode $ toS t0)
evalValue p (Object obj) = Object <$> evalValue p `traverse` obj
evalValue p (Array arr) = Array <$> evalValue p `traverse` arr
evalValue _ v = return v

decodeM :: (Monad m, FromJSON a) => B.ByteString -> ExceptT String m a
decodeM bs = case decodeEither bs of
  Right val -> return val
  Left err  -> throwError err

decodeWithEval :: FromJSON a => Plugin IO -> B.ByteString -> IO (Either String a)
decodeWithEval p bs = runExceptT $ do
  value <- decodeM bs
  res <- fromJSON <$> evalValue p value
  case res of
    Error msg -> throwError msg
    Success a -> return a
