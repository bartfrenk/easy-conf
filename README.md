# Extensible configuration language

## Description

Program to parse and execute statement in a simple YAML like format.

The main function is `decodeWithEval` in `Parse.hs`. Setting environment
variable `PORT` to 5000 and running `decodeWithEval` on the bytestring

```yaml
web:
  port: getEnv(PORT) or 8000
  host: getEnv(HOST) or localhost
```

gives value

```yaml
web:
    port: 5000
    host: localhost
```

## Desiderata

- finish Monoid instance for Plugin
- improve this README file
