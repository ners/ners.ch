#! /usr/bin/env nix
#! nix shell --impure --expr ``
#! nix with builtins;
#! nix let nixpkgs = getFlake "nixpkgs";
#! nix     pkgs = import nixpkgs {};
#! nix in pkgs.haskellPackages.ghcWithPackages (ps: with ps; [ JuicyPixels QuickCheck containers generic-lens hspec lens mtl parallel random random-fu  unordered-containers])
#! nix ``
#! nix --command runghc

{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoFieldSelectors #-}

import Codec.Picture.Gif
import Codec.Picture.Types
import Control.Lens.Operators
import Control.Monad (forM_, replicateM, replicateM_, unless)
import Control.Monad.Random.Class
import Control.Monad.State.Strict (evalState, execState)
import Control.Monad.State.Strict qualified as State
import Control.Parallel.Strategies
import Data.Either (either)
import Data.Foldable (toList)
import Data.Generics.Labels ()
import Data.HashSet qualified as HashSet
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.RVar (sampleStateRVar)
import Data.Random
import Data.Random.Distribution.Binomial
import Data.Ratio ((%))
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Traversable (for)
import Debug.Trace
import GHC.Conc (getNumCapabilities)
import GHC.Generics (Generic)
import GHC.TypeNats (KnownNat)
import Numeric.Natural
import System.Random
import System.Timeout (timeout)
import Test.HUnit.Base (assertFailure)
import Test.Hspec
import Test.QuickCheck

data DecayStrategy
    = OldestFirst
    | NewestFirst
    | RandomElements
    deriving (Bounded, Enum, Show)

data State a = State
    { stdGen :: StdGen
    , halfLife :: Double
    , decayStrategy :: DecayStrategy
    , elements :: Seq a
    }
    deriving (Generic)

instance Show (State a) where
    show State{..} =
        mconcat
            [ "State{halfLife="
            , show halfLife
            , ",decayStrategy=" <> show decayStrategy
            , ",elements="
            , show (Seq.length elements)
            , "}"
            ]

liftRandomGen :: (StdGen -> (b, StdGen)) -> State a -> (b, State a)
liftRandomGen f st = let (a, g) = f st.stdGen in (a, st{stdGen = g})

instance RandomGen (State a) where
    genWord8 = liftRandomGen genWord8
    genWord16 = liftRandomGen genWord16
    genWord32 = liftRandomGen genWord32
    genWord64 = liftRandomGen genWord64
    genWord32R = liftRandomGen . genWord32R
    genWord64R = liftRandomGen . genWord64R
    genShortByteString = liftRandomGen . genShortByteString
    split st = let (g1, g2) = split st.stdGen in (st{stdGen = g1}, st{stdGen = g2})

decay :: Double -> State a -> State a
decay dt = execState do
    State{..} <- State.get
    let k = halfLife / dt
        n = Seq.length elements
        p = 2 ** (-1 / k)
        q = 1 - p
    dn <- sampleState $ Binomial n q
    case decayStrategy of
        OldestFirst -> #elements %= Seq.drop dn
        NewestFirst -> #elements %= Seq.take (n - dn)
        RandomElements -> do
            indices <- sampleStateRVar $ shuffleNofM dn n [0 .. n - 1]
            #elements %= flip (foldr Seq.deleteAt) (reverse . List.sort $ indices)

instance (Arbitrary a) => Arbitrary (State a) where
    arbitrary = do
        halfLife <- choose (1e-6, 1e6)
        decayStrategy <- arbitraryBoundedEnum
        n <- chooseInt (0, 1000)
        elements <- Seq.fromList <$> replicateM n arbitrary
        pure State{stdGen = mkStdGen 0, ..}

data TestCase = TestCase {state :: State (), steps :: Natural}
    deriving (Show)

instance Arbitrary TestCase where
    arbitrary = do
        state <- arbitrary
        steps <- fromIntegral <$> chooseInteger (10, 100)
        pure TestCase{..}

data BinaryOp a = BinaryOp
    { op :: a -> a -> Bool
    , symbol :: String
    }

assertBinaryOp ::
    (HasCallStack, Show a) =>
    -- | The message prefix
    String ->
    BinaryOp a ->
    -- | The expected value
    a ->
    -- | The actual value
    a ->
    IO ()
assertBinaryOp preface BinaryOp{..} a b =
    unless (op a b) (assertFailure msg)
  where
    msg =
        (if null preface then "" else preface ++ "\n")
            ++ "expected "
            ++ show a
            ++ " "
            ++ symbol
            ++ " "
            ++ show b

shouldBeLT :: (HasCallStack, Ord a, Show a) => a -> a -> Expectation
shouldBeLT = assertBinaryOp "" (BinaryOp (<) "<")

shouldBeLTE :: (HasCallStack, Ord a, Show a) => a -> a -> Expectation
shouldBeLTE = assertBinaryOp "" (BinaryOp (<=) "<=")

shouldBeGT :: (HasCallStack, Ord a, Show a) => a -> a -> Expectation
shouldBeGT = assertBinaryOp "" (BinaryOp (>) ">")

shouldBeGTE :: (HasCallStack, Ord a, Show a) => a -> a -> Expectation
shouldBeGTE = assertBinaryOp "" (BinaryOp (>=) ">=")

shouldBeEQ :: (HasCallStack, Ord a, Show a) => a -> a -> Expectation
shouldBeEQ = assertBinaryOp "" (BinaryOp (==) "==")

decaysCorrectly :: TestCase -> Expectation
decaysCorrectly TestCase{..} = fromRational (abs avgDeviation) `shouldBeLT` (10 * sigma + eps)
  where
    dt = state.halfLife / fromIntegral steps
    n0 = fromIntegral $ Seq.length state.elements
    seeds = Seq.fromList [0 .. 1000]
    deviations = flip fmap seeds $ \seed -> flip evalState state do
        #stdGen .= mkStdGen seed
        replicateM_ (fromIntegral steps) . State.modify . decay $ abs dt
        actualN <- fromIntegral <$> State.gets (Seq.length . (.elements))
        pure $ n0 `div` 2 - actualN
    cleanedDeviations = Seq.drop 3 $ Seq.reverse $ Seq.drop 3 $ Seq.sort deviations
    avgDeviation = sum cleanedDeviations % fromIntegral (Seq.length cleanedDeviations)
    variance = n0 % fromIntegral (4 * length deviations)
    sigma = sqrt $ fromRational variance
    eps = 1e-6

spec :: Spec
spec = it "decays half of the remaining elements every half-life" . property $ decaysCorrectly

main :: IO ()
main = do
    hspec spec
    forM_ [minBound .. maxBound] \decayStrategy -> do
        print decayStrategy
        let imageWidth = 50
        let imageHeight = 50
        let pixelScale = 20
        let state =
                State
                    { stdGen = mkStdGen 0
                    , halfLife = 2
                    , decayStrategy
                    , elements = Seq.fromList [(x, y) | y <- [0 .. imageHeight - 1], x <- [0 .. imageWidth - 1]]
                    }
        let duration = 10
        let dt = 0.05
        let steps = round $ duration / dt
        let pixelAt s x y = if HashSet.member (x `div` pixelScale, y `div` pixelScale) s then 20 else 200
        let frame State{elements} =
                let set = HashSet.fromList $ toList elements
                 in generateImage
                        (pixelAt set)
                        (imageWidth * pixelScale)
                        (imageHeight * pixelScale)
        let frames = frame <$> iterate (decay dt) state

        either error id . writeGifImages ("Decay-" <> show decayStrategy <> ".gif") LoopingForever $
            zipWith (\i -> trace ("Generating frame " <> show i <> " of " <> show steps)) [1 ..] [(greyPalette, round $ dt * 100, frame) | frame <- take steps frames]
