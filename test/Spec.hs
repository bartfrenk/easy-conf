import           Control.Monad.Catch  (MonadThrow, SomeException)
import           Control.Monad.Reader
import           Data.Aeson           (FromJSON)
import           Data.Bifunctor       (first)
import           Data.ByteString      (ByteString)
import           Data.Either          (isLeft)
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Text            (Text)
import           GHC.Generics         (Generic)
import           Test.Hspec

import           Data.Config


type Env = Map String String

newtype EnvReader a = EnvReader (ReaderT Env (Either SomeException) a)
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadThrow)

runEnvReader :: Env -> EnvReader a -> Either String a
runEnvReader env (EnvReader act) = first show (runReaderT act env)

instance MonadEnv EnvReader where
  lookupEnv key = asks (Map.lookup key)

decode :: FromJSON a => Env -> ByteString -> Either String a
decode env bs = runEnvReader env (decodeWithDefault bs)

decodeNested :: FromJSON a => Env -> [Text] -> ByteString -> Either String a
decodeNested env keys bs =
  let act = throwDecode bs >>= decodeWithPluginNested defaultPlugin keys
  in runEnvReader env act

defaultEnv :: Env
defaultEnv = Map.fromList [("PORT", "8080"), ("HOST", "10.10.0.1")]

data Settings = Settings
  { host :: String
  , port :: Maybe Int
  } deriving (Eq, Show, Generic)

instance FromJSON Settings

main :: IO ()
main = hspec $ do

  describe "decodeWithDefaultPlugin" $ do

    it "parses YAML without macros normally" $ do
      let settings = "host: localhost\nport: 5000"
      decode defaultEnv settings `shouldBe` mkSettings "localhost" 5000

    it "replaces $env macros by their environment values" $ do
      let settings = "host: $env(HOST) or localhost\nport: 5000"
      decode defaultEnv settings `shouldBe` mkSettings "10.10.0.1" 5000

    it "transforms replaced values to the right type" $ do
      let settings = "host: localhost or localhost\nport: $env(PORT) or 5000"
      decode defaultEnv settings `shouldBe` mkSettings "localhost" 8080

    it "ignores non-existing environment variables" $ do
      let settings = "host: localhost or localhost\nport: $env(SPORT) or 5000"
      decode defaultEnv settings `shouldBe` mkSettings "localhost" 5000

    it "uses the first non-null value" $ do
      let settings = "host: localhost or $env(HOST) or localhost\nport: 5000"
      decode defaultEnv settings `shouldBe` mkSettings "localhost" 5000

    it "fails when a value cannot be determined for a non-Maybe type" $ do
      let settings = "host: $env(SHOST)\nport: 5000"
      decode defaultEnv settings `shouldSatisfy` isFailure

    it "succeeds when a value cannot be determined for a Maybe type" $ do
      let settings = "host: localhost\nport: $env(SPORT)"
      decode defaultEnv settings
        `shouldBe` (Right $ Settings { host = "localhost"
                                     , port = Nothing
                                     })

  describe "decodeWithPluginNested" $ do

    it "parses a nested structure without macros" $ do
      let settings = "server:\n  host: localhost\n  port: 5000\n\
                     \database:\n  host: 10.10.0.10\n  port: 5432"
      decodeNested defaultEnv ["server"] settings
        `shouldBe` mkSettings "localhost" 5000

    it "parses a nested structure with macros" $ do
      let settings = "server:\n  host: $env(HOST) or localhost\n  port: 5000"
      decodeNested defaultEnv ["server"] settings
        `shouldBe` mkSettings "10.10.0.1" 5000

  where

    mkSettings host port = Right $ Settings
      { host = host,
        port = Just port
      }

    isFailure :: Either String Settings -> Bool
    isFailure = isLeft
