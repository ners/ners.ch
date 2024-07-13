#! /usr/bin/env nix
#! nix shell --impure --expr ``
#! nix with builtins;
#! nix let nixpkgsFlake = getFlake "nixpkgs";
#! nix     rhineFlake = getFlake "github:ners/rhine/flake";
#! nix     pkgs = import nixpkgsFlake { overlays = [ rhineFlake.outputs.overlays.default ]; };
#! nix in pkgs.haskellPackages.ghcWithPackages (ps: with ps; [ QuickCheck containers rhine random random-fu ])
#! nix ``
#! nix --command runghc

{-# LANGUAGE GHC2021 #-}

import Data.Random.Distribution.Binomial
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import FRP.Rhine
import System.Random

data DecayStrategy
    = OldestFirst
    | NewestFirst
    | RandomElements

data State = State
    { stdGen :: StdGen
    , halfLife :: Double
    , decayStrategy :: DecayStrategy
    , elements :: Seq Int
    }

decay :: (Monad m, Diff (Time cl) ~ Double) => ClSF m cl State State
decay = returnA

main :: IO ()
main = do
    stdGen <- initStdGen
    let state =
            State
                { stdGen
                , halfLife = 5
                , decayStrategy = OldestFirst
                , elements = Seq.fromList [1 .. 1000]
                }
    pure ()
