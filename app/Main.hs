module Main where

import qualified Data.ByteString      as B
import Data.Aeson
import System.Environment (setEnv)

import Eval

printValue :: Value -> IO ()
printValue = print . encode

main :: IO ()
main = do
  bs <- B.readFile "res/test.yaml"
  setEnv "PORT" "5000"
  result <- decodeWithEval envPlugin bs
  case result of
    Left err -> putStrLn err
    Right v -> printValue v


