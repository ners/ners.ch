# Reversible parsing

Reversible parsing libries such as [syntax] allow us to write a single syntax description and instantiate it both as a parser and a pretty printer.

[syntax]: https://hackage.haskell.org/package/syntax

> [!info]
> syntax has been updated for modern GHCs here: [syntax](https://github.com/ners/syntax) [semi-iso](https://github.com/ners/semi-iso)

## Motivation

Consider the following data type:

```haskell
data Platform = Plaftorm Architecture Kernel

data Architecture
    = Amd64
    | Arm64
    | RiscV64

data Kernel
    = Darwin
    | Linux
    | Windows
```

With this type we can represent a subset of [[Nix]] platform strings:

```nix
[
  "aarch64-darwin"
  "aarch64-linux"
  "armv6l-linux"
  "armv7l-linux"
  "i686-linux"
  "powerpc64le-linux"
  "riscv64-linux"
  "x86_64-darwin"
  "x86_64-freebsd"
  "x86_64-linux"
  ...
]
```

Here is a naïve approach to serialising and deserialising these types:

```haskell
printArchitecture :: Architecture -> String
printArchitecture Amd64 = "x86_64"
printArchitecture Arm64 = "aarch64"
printArchitecture RiscV64 = "riscv64"

parseArchitecture :: String -> Maybe Architecture
parseArchitecture "x86_64" = Just Amd64
parseArchitecture "aarch64" = Just Arm64
parseArchitecture "riscv64" = Just RiscV64
parseArchitecture _ = Nothing

printKernel :: Kernel -> String
printKernel Darwin = "darwin"
printKernel Linux = "linux"
printKernel Windows = "windows"

parseKernel :: String -> Maybe Kernel
parseKernel "darwin" = Just Darwin
parseKernel "linux" = Just Linux
parseKernel "windows" = Just Windows
parseKernel _ = Nothing

printPlatform :: Platform -> String
printPlatform (Platform arch kernel) =
    mconcat
        [ printArchitecture arch
        , "-"
        , printKernel kernel
        ]

parsePlatform :: String -> Maybe Platform
parsePlatform str =
    case Data.List.Extra.split (== '-') str of
        [archStr, kernelStr] -> do
            arch <- parseArchitecture archStr
            kernel <- parseKernel kernelStr
            pure $ Platform arch kernel
        _ -> Nothing
```

This code has quite a bit of duplication. We can reduce the duplication by expressing sum type `parse` in terms of `print`:
```haskell
parseArchitecture :: String -> Maybe Architecture
parseArchitecture str = find (\a -> printArchitecture a == str) [Amd64, Arm64, RiscV64]

parseKernel :: String -> Maybe Kernel
parseKernel str = find (\k -> printKernel k == str) [Darwin, Linux, Windows]
```

And even further if we can derive `Bounded` and `Enum` on our sum types:

```haskell
parseArchitecture :: String -> Maybe Architecture
parseArchitecture str = find (\a -> printArchitecture a == str) [minBound .. maxBound]

parseKernel :: String -> Maybe Kernel
parseKernel str = find (\k -> printKernel k == str) [minBound .. maxBound]
```

Is it possible to express `parsePlatform` in terms of `printPlatform` and deduplicate the special handling of `'-'`?

In Haskell, the answer is always *yes*[^1]!

[^1]: Often short for *yes, but ...*

## One syntax to rule them all

```haskell
{-# LANGUAGE TemplateHaskell #-}

import Control.Lens.TH (makePrisms)
import Control.SIArrow ((/$/), (/$~/), (/*/), (/+/))
import Data.Syntax qualified as S
import Data.Syntax.Char (SyntaxText)

$(makePrisms ''Architecture)
$(makePrisms ''Kernel)
$(makePrisms ''Platform)

architecture :: (SyntaxText syn) => syn () Architecture
architecture =
    _Amd64 /$/ S.string "x86_64"
    /+/ _Arm64 /$/ S.string "aarch64"
    /+/ _RiscV64 /$/ S.string "riscv64"

kernel :: (SyntaxText syn) => syn () Kernel
kernel =
    _Darwin /$/ S.string "darwin"
    /+/ _Linux /$/ S.string "linux"
    /+/ _Windows /$/ S.string "windows"

platform :: (SyntaxText syn) => syn () Platform
platform = _Platform /$~ architecture /*/ S.char '-' /*/ kernel
```

We can now parse and pretty-print platform strings without any repetition:

```haskell
import Data.Attoparsec.Text.Lazy qualified as AP
import Data.Syntax.Attoparsec.Text.Lazy qualified as S
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder qualified as T

parsePlatform :: Text -> Either String Platform
parsePlatform t =
    case AP.parse (S.getParser_ platform <* AP.skipSpace <* AP.endOfInput) t of
        AP.Fail _ _ err -> Left err
        AP.Done _ p -> Right p

printPlatform :: Platform -> Text
printPlatform = T.toLazyText . fromRight . S.runPrinter_ platform
```
