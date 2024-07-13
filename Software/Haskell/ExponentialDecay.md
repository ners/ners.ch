# Exponential decay

We wish to simulate [exponential decay] on a set of discrete elements over time.

We run our simulation at a frequency of $\frac{1\ \text{s}}{\delta t}\ \text{Hz}$. Each simulation step happens $\delta t$ after the previous.

Our simulation speed relates to the half-life $t_{1/2}$ with the coefficient $k =\frac{t_{1/2}}{\delta t}$.

On average, we expect to decay half of the remaining elements every $t_{1/2}$, or on every $k$ simulation steps.

On each simulation step we expect to decay $N_t (1 - 2^{-1/k})$, where $N_t$ is the number of remaining elements.

Proof:

$$
\begin{split}
decay(N) & = N - N (1 - 2^{-1/k}) \\
         & = N\ 2^{-1/k} \\
decay^{\circ k}(N) & = N\ 2^{-k/k} = \frac{N}{2}\ \square
\end{split}
$$

At each simulation step, each element has a probability $p = 2^{-1/k}$ to survive the step, and a probability $q = 1-p$ to decay.

Finally, on each simulation step, we decay $\delta N$ of the remaining elements. $\delta N$ is a [binomially distributed][binomial distribution] random variable:

$$
\begin{split}
\delta N & \sim B(N_t, q)
\\
E[\delta N] & = N_t\ q = N_t (1 - 2^{-1/k})
\end{split}
$$

[exponential decay]: https://en.wikipedia.org/wiki/Exponential_decay
[binomial distribution]: https://en.wikipedia.org/wiki/Binomial_distribution

## Talk is cheap, show me the code

Here is an abridged snippet that shows the main logic:

```haskell
import Control.Lens.Operators
import Control.Monad.Random.Class
import Control.Monad.State.Strict qualified as State
import Data.Generics.Labels ()
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import System.Random

data DecayStrategy
    = OldestFirst
    | NewestFirst
    | RandomElements

data State a = State
    { stdGen :: StdGen
    , halfLife :: Double
    , decayStrategy :: DecayStrategy
    , elements :: Seq a
    }
    deriving (Generic)

decay :: Double -> State a -> State a
decay dt = State.execState do
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
```

Let's test it:

```haskell
import Test.Hspec
import Test.QuickCheck

data TestCase = TestCase {state :: State (), steps :: Natural}
    deriving (Show)

instance Arbitrary TestCase where
    arbitrary = do
        state <- arbitrary
        steps <- fromIntegral <$> chooseInteger (10, 100)
        pure TestCase{..}

instance (Arbitrary a) => Arbitrary (State a) where
    arbitrary = do
        halfLife <- choose (1e-6, 1e6)
        decayStrategy <- arbitraryBoundedEnum
        n <- chooseInt (0, 1000)
        elements <- Seq.fromList <$> replicateM n arbitrary
        pure State{stdGen = mkStdGen 0, ..}

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
```

Let's plot it:

```haskell
import Codec.Picture.Gif
import Codec.Picture.Types

main :: IO ()
main =
    either error id . writeGifImages "decay.gif" LoopingForever $
        [(greyPalette, round $ dt * 100, frame) | frame <- take steps frames]
  where
    imageWidth = 50
    imageHeight = 50
    pixelScale = 20
    state =
        State
            { stdGen = mkStdGen 0
            , halfLife = 2
            , decayStrategy = OldestFirst
            , elements = Seq.fromList [(x, y) | y <- [0 .. imageHeight - 1], x <- [0 .. imageWidth - 1]]
            }
    duration = 10
    dt = 0.05
    steps = round $ duration / dt
    pixelAt s x y = if HashSet.member (x `div` pixelScale, y `div` pixelScale) s then 20 else 200
    frame State{elements} =
        let set = HashSet.fromList $ toList elements
         in generateImage
                (pixelAt set)
                (imageWidth * pixelScale)
                (imageHeight * pixelScale)
    frames = frame <$> iterate (decay dt) state
```

<figure style="margin:1em 0;">
    <p style="display:flex;flex-direction:row;gap:1em;">
        <img style="max-height:10em;" src="/Software/Haskell/ExponentialDecay/OldestFirst.gif"/>
        <img style="max-height:10em;" src="/Software/Haskell/ExponentialDecay/NewestFirst.gif"/>
        <img style="max-height:10em;" src="/Software/Haskell/ExponentialDecay/RandomElements.gif"/>
    </p>
</figure>

Rendering 10 s of decay using the `OldestFirst`, `NewestFirst`, and `RandomElements` strategies with $N_0 = 2500, t_{1/2}=2\ \text{s}, dt = 0.05\ \text{s}$. Black pixels represent remaining elements arranged in a 2D grid.

> [!note]
> You may read the full unabridged program here: [[ExponentialDecay.hs]]
