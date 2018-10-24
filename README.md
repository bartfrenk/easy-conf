# Extensible configuration language

## Description

Simple configuration language for Haskell programs based on YAML and inspired by
EDN. See the next section for an example on how to use it.

## Example

Given a file `test.yaml` with the following content:

```yaml
web:
  port: $env(PORT) or 8000
  host: localhost
```

The program:

```haskell
import           Data.Aeson
import qualified Data.ByteString    as B
import           System.Environment (setEnv)

import           Data.Config (decodeWithDefault)

data Settings = Settings
  { host :: String
  , port :: Maybe Int
  } deriving (Eq, Show, Generic)

instance FromJSON Settings

readSettings :: IO Settings
readSettings = do
  setEnv "PORT" "5000"
  bs <- B.readFile "test.yaml"
  decodeWithDefault bs

main :: IO ()
main = readSettings >>= print
```

prints:

```haskell
Settings { port = Just 5000, host = "localhost" }
```

