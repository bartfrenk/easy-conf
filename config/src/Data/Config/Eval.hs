module Data.Config.Eval
  ( evalExpr
  , evalValue
  , throwDecode
  ) where

import           Control.Monad.Catch (Exception, MonadThrow, throwM)
import           Data.Aeson          hiding (decode)
import qualified Data.ByteString     as B
import           Data.Text           (Text)
import           Data.Text           (pack)
import           Data.Typeable
import           Data.Yaml

import           Data.Config.Expr    (Expr (..), parseExpr)
import           Data.Config.Plugin  (Plugin, throwRunPlugin)

data EvalException
  = DecodingFailed String
  deriving (Show, Typeable)

instance Exception EvalException

evalExpr :: MonadThrow m => Plugin m -> Expr -> m (Maybe Text)
evalExpr p (TmFunc name arg) = throwRunPlugin p name arg
evalExpr _ (TmLit s) = pure $ Just $ pack s
evalExpr p (TmOr e f) = evalExpr p e >>= \case
  Just value -> pure $ Just value
  Nothing -> evalExpr p f

evalValue :: MonadThrow m => Plugin m -> Value -> m Value
evalValue plugin (String txt) = parseExpr txt >>= evalExpr plugin >>= \case
  Nothing -> pure $ String txt
  Just txt' -> pure $ String txt'
evalValue plugin (Object obj) = Object <$> evalValue plugin `traverse` obj
evalValue plugin (Array arr) = Array <$> evalValue plugin `traverse` arr
evalValue _ value = return value

throwDecode :: (MonadThrow m, FromJSON a) => Plugin m -> B.ByteString -> m a
throwDecode plugin bs =
  fromJSON <$> (throwDecode bs >>= evalValue plugin) >>= \case
    Error msg -> throwM $ DecodingFailed msg
    Success a -> return a
  where
    throwDecode bs =
      case decodeEither bs of
        Right value -> pure value
        Left msg -> throwM $ DecodingFailed msg
