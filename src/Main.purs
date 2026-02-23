module Main where

import Prelude

import Control.Comonad (class Comonad, class Extend, extend, extract)
import Control.Monad.Gen.Class (class MonadGen, chooseFloat)
import Control.Monad.Gen.Trans (Gen, evalGen, shuffle)
import Data.Array (concatMap, filter, index, length, nub, range, replicate, slice, snoc, sort, zipWith)
import Data.Bifunctor (lmap)
import Data.Int (toNumber)
import Data.Char (toCharCode)
import Data.Foldable (foldl, sum, surroundMap)
import Data.Function (on)
import Data.Graph.Weighted.DAG (DAG, topologicalSort)
import Data.Map (Map, fromFoldableWith, lookup, singleton, unionWith)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Number as N
import Data.String (Pattern(..), null, split, trim)
import Data.String.CodeUnits (toCharArray)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (replicateA)
import Data.Newtype (class Newtype, unwrap)
import Partial.Unsafe (unsafePartial)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Random.LCG (randomSeed)

data Expr a
  = Val a
  | Add a (Expr a) (Expr a)
  | Mul a (Expr a) (Expr a)
  | Pow a (Expr a) Number
  | Exp a (Expr a)
  | Log a (Expr a)
  | Relu a (Expr a)

derive instance Eq a => Eq (Expr a)
derive instance Ord a => Ord (Expr a)
derive instance Functor Expr

instance Show a => Show (Expr a) where
  show (Val v) = "(Val " <> show v <> ")"
  show (Add v a b) = "(Add " <> show v <> " " <> show a <> " " <> show b <> ")"
  show (Mul v a b) = "(Mul " <> show v <> " " <> show a <> " " <> show b <> ")"
  show (Pow v a n) = "(Pow " <> show v <> " " <> show a <> " " <> show n <> ")"
  show (Exp v a) = "(Exp " <> show v <> " " <> show a <> ")"
  show (Log v a) = "(Log " <> show v <> " " <> show a <> ")"
  show (Relu v a) = "(Relu " <> show v <> " " <> show a <> ")"

instance Extend Expr where
  extend f expr@(Val _) = Val (f expr)
  extend f expr@(Add _ a b) = Add (f expr) (extend f a) (extend f b)
  extend f expr@(Mul _ a b) = Mul (f expr) (extend f a) (extend f b)
  extend f expr@(Pow _ a n) = Pow (f expr) (extend f a) n
  extend f expr@(Exp _ a) = Exp (f expr) (extend f a)
  extend f expr@(Log _ a) = Log (f expr) (extend f a)
  extend f expr@(Relu _ a) = Relu (f expr) (extend f a)

instance Comonad Expr where
  extract (Val v) = v
  extract (Add v _ _) = v
  extract (Mul v _ _) = v
  extract (Pow v _ _) = v
  extract (Exp v _) = v
  extract (Log v _) = v
  extract (Relu v _) = v

type GradMap = Map (Expr Number) Number

propagate :: Number -> Expr Number -> Array (Expr Number /\ Number)
propagate g = case _ of
  Val _ -> []
  -- ∂(a+b)/∂a = 1, ∂(a+b)/∂b = 1
  Add _ a b -> [ a /\ g, b /\ g ]
  -- ∂(a·b)/∂a = b, ∂(a·b)/∂b = a
  Mul _ a b -> [ a /\ (g * extract b), b /\ (g * extract a) ]
  -- ∂aⁿ/∂a = n·aⁿ⁻¹
  Pow _ a n -> [ a /\ (g * n * N.pow (extract a) (n - 1.0)) ]
  -- ∂eᵃ/∂a = eᵃ
  Exp v a -> [ a /\ (g * v) ]
  -- ∂(ln a)/∂a = 1/a
  Log _ a -> [ a /\ (g / extract a) ]
  -- ∂max(0,a)/∂a = 1 if a>0, else 0
  Relu _ a -> [ a /\ (g * if extract a > 0.0 then 1.0 else 0.0) ]

backward :: Expr Number -> DAG (Expr Number) Unit -> GradMap
backward root dag = foldl step (singleton root 1.0) (topologicalSort dag)
  where
  step :: GradMap -> Expr Number -> GradMap
  step grads expr = fromMaybe grads do
    g <- lookup expr grads
    pure $ unionWith (+) grads (fromFoldableWith (+) $ propagate g expr)

encode :: Char -> Int
encode c = on (-) toCharCode c 'a'

buildVocab :: Array String -> Array Char
buildVocab = sort <<< nub <<< concatMap toCharArray

tokenize :: Array String -> Array Int
tokenize docs = surroundMap [ bos ] (map encode <<< toCharArray) docs
  where
  bos = length $ buildVocab docs

initDataset :: String -> Gen (Array String)
initDataset content = shuffle docs
  where
  docs = filter (not <<< null) $ trim <$> split (Pattern "\n") content

sampleGauss :: forall m. MonadGen m => Number -> m Number
sampleGauss std = do
  u1 <- chooseFloat 1.0e-7 1.0
  u2 <- chooseFloat 0.0 1.0
  let z = N.sqrt (-2.0 * N.log u1) * N.cos (2.0 * N.pi * u2)
  pure $ z * std

matrix :: Int -> Int -> Gen (Matrix (Expr Number))
matrix nout nin = replicateA nout $ replicateA nin do
  g <- sampleGauss std
  pure $ Val g
  where
  std = 0.08

type RowVec a = Array a
type ColVec a = Array a
type Matrix a = Array (Array a)

type LayerWeights =
  { attnWq :: Matrix (Expr Number)
  , attnWk :: Matrix (Expr Number)
  , attnWv :: Matrix (Expr Number)
  , attnWo :: Matrix (Expr Number)
  , mlpFc1 :: Matrix (Expr Number)
  , mlpFc2 :: Matrix (Expr Number)
  }

type StateDict =
  { wte :: Matrix (Expr Number)
  , wpe :: Matrix (Expr Number)
  , lmHead :: Matrix (Expr Number)
  , layers :: Array LayerWeights
  }

initParams :: Int -> Int -> Int -> Int -> Int -> Gen StateDict
initParams nEmbd nHead nLayer blockSize vocabSize = do
  wte <- matrix vocabSize nEmbd
  wpe <- matrix blockSize nEmbd
  lmHead <- matrix vocabSize nEmbd
  layers <- replicateA nLayer do
    attnWq <- matrix nEmbd nEmbd
    attnWk <- matrix nEmbd nEmbd
    attnWv <- matrix nEmbd nEmbd
    attnWo <- matrix nEmbd nEmbd
    mlpFc1 <- matrix (4 * nEmbd) nEmbd
    mlpFc2 <- matrix nEmbd (4 * nEmbd)
    pure { attnWq, attnWk, attnWv, attnWo, mlpFc1, mlpFc2 }
  pure { wte, wpe, lmHead, layers }

flatten :: StateDict -> Array (Expr Number)
flatten sd = join (join <$> matrices)
  where
  matrices = [ sd.wte, sd.wpe, sd.lmHead ]
    <> concatMap layerMatrices sd.layers
  layerMatrices l = [ l.attnWq, l.attnWk, l.attnWv, l.attnWo, l.mlpFc1, l.mlpFc2 ]

dot :: RowVec Number -> ColVec Number -> Number
dot u v = sum $ zipWith (*) u v

linear :: Matrix Number -> ColVec Number -> ColVec Number
linear w x = w <#> \row -> dot row x

softmax :: Array Number -> Array Number
softmax logits = (_ / sum exps) <$> exps
  where
  maxVal = foldl max (-N.infinity) logits
  exps = (N.exp <<< (_ - maxVal)) <$> logits

rmsnorm :: Array Number -> Array Number
rmsnorm x = ((*) scale) <$> x
  where
  ms = sum (square <$> x) / toNumber (length x)
  scale = N.pow (ms + 1e-5) (-0.5)
  square = join mul

relu :: Number -> Number
relu = max 0.0

withResidual :: forall f. Functor f => (ColVec Number -> f (ColVec Number)) -> ColVec Number -> f (ColVec Number)
withResidual f x = map (zipWith (+) x) (f $ rmsnorm x)

newtype TokenId = TokenId Int
newtype PosId = PosId Int

type Query = ColVec Number
type Key = ColVec Number
type Value = ColVec Number

newtype KVCache = KVCache
  { keys :: Array Key
  , values :: Array Value
  }

derive instance Newtype KVCache _

instance Semigroup KVCache where
  append (KVCache c1) (KVCache c2) =
    KVCache { keys: c1.keys <> c2.keys, values: c1.values <> c2.values }

instance Monoid KVCache where
  mempty = KVCache { keys: [], values: [] }

embedding :: Matrix Number -> Matrix Number -> TokenId -> PosId -> ColVec Number
embedding wte wpe (TokenId tokId) (PosId posId) = zipWith (+) tokEmb posEmb
  where
  tokEmb = unsafePartial $ fromJust $ index wte tokId
  posEmb = unsafePartial $ fromJust $ index wpe posId

headAttn :: Int -> Int -> Query -> Array Key -> Array Value -> ColVec Number
headAttn h headDim q keys values = headOut
  where
  hs = h * headDim
  qH = slice hs (hs + headDim) q
  kH = slice hs (hs + headDim) <$> keys
  vH = slice hs (hs + headDim) <$> values
  attnLogits = (\k -> dot qH k / N.pow (toNumber headDim) 0.5) <$> kH
  attnWeights = softmax attnLogits
  headOut = foldl (zipWith (+)) (replicate headDim 0.0) (zipWith (\w v -> (_ * w) <$> v) attnWeights vH)

multiHeadAttn :: LayerWeights -> Int -> KVCache -> ColVec Number -> KVCache /\ ColVec Number
multiHeadAttn weights headDim cache x = cache' /\ x'
  where
  attnWq = (map extract) <$> weights.attnWq
  attnWk = (map extract) <$> weights.attnWk
  attnWv = (map extract) <$> weights.attnWv
  attnWo = (map extract) <$> weights.attnWo

  q = linear attnWq x
  k = linear attnWk x
  v = linear attnWv x

  cache' = KVCache { keys: snoc (unwrap cache).keys k, values: snoc (unwrap cache).values v }

  nHead = length q / headDim
  xAttn = concatMap (\h -> headAttn h headDim q (unwrap cache').keys (unwrap cache').values) (range 0 $ nHead - 1)
  x' = linear attnWo xAttn

mlp :: LayerWeights -> ColVec Number -> ColVec Number
mlp weights = linear fc2 <<< map relu <<< linear fc1
  where
  fc1 = (map extract) <$> weights.mlpFc1
  fc2 = (map extract) <$> weights.mlpFc2

gpt :: StateDict -> Int -> Array KVCache -> TokenId -> PosId -> Array Number /\ Array KVCache
gpt sd headDim caches tokId posId = logits /\ caches'
  where
  wte = (map extract) <$> sd.wte
  wpe = (map extract) <$> sd.wpe
  lmHead = (map extract) <$> sd.lmHead
  x = embedding wte wpe tokId posId
  step (cs /\ v) (w /\ c) = lmap (snoc cs) $ (withResidual (multiHeadAttn w headDim c) >=> withResidual (\y -> mempty /\ mlp w y)) v
  caches' /\ x' = foldl step ([] /\ x) (zipWith (/\) sd.layers caches)
  logits = linear lmHead x'

main :: Effect Unit
main = launchAff_ do
  content <- readTextFile UTF8 "src/input.txt"
  seed <- liftEffect randomSeed
  let
    (dataset /\ params) = flip evalGen { newSeed: seed, size: 0 } do
      dataset <- initDataset content
      let vocabSize = length (buildVocab dataset) + 1
      params <- initParams 16 4 1 16 vocabSize
      pure $ dataset /\ params
  pure unit
