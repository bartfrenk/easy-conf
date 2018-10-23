# Extensible configuration language

## Description

Simple configuration language for Haskell programs based on YAML and inspired by
EDN. See the next section for an example on how to use it.

## Example

Given a file `test.yaml` with the following content:

```yaml
web:
  port: $env(PORT) or 8000
  host: $env(HOST) or localhost
```

The program:

```haskell
import           Data.Aeson
import qualified Data.ByteString    as B
import           System.Environment (setEnv)

import           Data.Config (throwDecode, defaultPlugin)

printValue :: Value -> IO ()
printValue = print . encode

main :: IO ()
main = do
  setEnv "PORT" "5000"

  bs <- B.readFile "test.yaml"
  value <- throwDecode defaultPlugin bs
  printValue value
```

prints the YAML object:

```yaml
web:
  port: 5000
  host: localhost
```

