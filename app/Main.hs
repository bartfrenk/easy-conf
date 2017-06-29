module Main where

import           Data.Aeson
import qualified Data.ByteString    as B
import           System.Environment (setEnv)

import           Eval
import           Plugin

printValue :: Value -> IO ()
printValue = print . encode

main :: IO ()
main = do
  bs <- B.readFile "res/test.yaml"
  setEnv "PORT" "5000"
  result <- decodeWithEval (envPlugin <> idPlugin) bs
  case result of
    Left err -> putStrLn err
    Right v  -> printValue v


