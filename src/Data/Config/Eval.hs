module Data.Config.Eval
  ( evalExpr
  , evalValue
  , decodeWithPlugin
  , decodeWithPluginNested
  , throwDecode
  ) where

import           Control.Monad.Catch (Exception, MonadThrow, throwM)
import           Data.Aeson          hiding (decode)
import qualified Data.ByteString     as B
import qualified Data.HashMap.Strict as Map
import           Data.String.Conv    (toS)
import           Data.Text           (Text)
import           Data.Text           (pack)
import           Data.Typeable
import           Data.Yaml

import           Data.Config.Expr    (Expr (..), parseExpr)
import           Data.Config.Plugin  (Plugin, throwRunPlugin)

data EvalException
  = DecodingFailed String
  | NoSuchKey Text
  | NoValue
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
  Nothing -> pure $ Null
  Just txt' -> throwDecode $ toS txt'
evalValue plugin (Object obj) = Object <$> evalValue plugin `traverse` obj
evalValue plugin (Array arr) = Array <$> evalValue plugin `traverse` arr
evalValue _ value = pure value

decodeWithPlugin :: (MonadThrow m, FromJSON a) => Plugin m -> B.ByteString -> m a
decodeWithPlugin plugin bs = throwDecode bs >>= evalValue plugin >>= throwFromJSON

decodeWithPluginNested :: (MonadThrow m, FromJSON a)
                       => Plugin m -> [Text] -> Value -> m a
decodeWithPluginNested plugin = recur
  where
    recur [] val = evalValue plugin val >>= throwFromJSON
    recur (key:rest) (Object obj) =
      case Map.lookup key obj of
        Nothing -> throwM $ NoSuchKey key
        Just val' -> recur rest val'
    recur (key:_) _ = throwM $ NoSuchKey key

throwFromJSON :: (MonadThrow m, FromJSON a) => Value -> m a
throwFromJSON val =
  case fromJSON val of
    Error msg -> throwM $ DecodingFailed msg
    Success a -> pure a

throwDecode :: (MonadThrow m, FromJSON a) => B.ByteString -> m a
throwDecode bs =
  case decodeEither bs of
    Right value -> pure value
    Left msg -> throwM $ DecodingFailed msg




