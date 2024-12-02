#! /usr/bin/env nix
#! nix shell --impure --expr ``
#! nix with builtins;
#! nix let nixpkgs = getFlake "nixpkgs";
#! nix     syntax = getFlake "github:ners/syntax";
#! nix     pkgs = import nixpkgs { overlays = [ syntax.outputs.overlays.default ]; };
#! nix in pkgs.haskellPackages.ghcWithPackages (ps: with ps; [ attoparsec lens syntax syntax-attoparsec syntax-printer text ])
#! nix ``
#! nix --command runghc

{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Category.Structures
import Control.Lens.SemiIso (ASemiIso', semiIso)
import Control.Lens.TH (makePrisms)
import Control.SIArrow
import Data.Attoparsec.Text (parseOnly)
import Data.Functor ((<&>))
import Data.Syntax qualified as S
import Data.Syntax.Attoparsec.Text (getParser_)
import Data.Syntax.Char (SyntaxText)
import Data.Syntax.Printer.Text (runPrinter_)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)

data Architecture
    = Amd64
    | Arm64
    | RiscV64
    deriving (Eq)

data Kernel
    = Darwin
    | Linux
    | Windows
    deriving (Eq)

data Platform = Platform Architecture Kernel
    deriving (Eq)

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

validPlatform :: ASemiIso' Platform (Architecture, Kernel)
validPlatform = semiIso fromPlatform toPlatform
    where
        invalidPlatforms = [ (RiscV64, Darwin) ]
        toPlatform (a, k)
            | (a, k) `elem` invalidPlatforms = Left "invalid platform"
            | otherwise = Right $ Platform a k
        fromPlatform (Platform a k) = toPlatform (a, k) >> Right (a, k)

platform :: (SyntaxText syn) => syn () Platform
platform = validPlatform /$~ architecture /*/ S.char '-' /*/ kernel

parsePlatform :: Text -> Either String Platform
parsePlatform = parseOnly $ getParser_ platform

printPlatform :: Platform -> Either String Text
printPlatform = fmap (toStrict . toLazyText) . runPrinter_ platform

main :: IO ()
main = print $ printPlatform =<< parsePlatform "x86_64-linux"
