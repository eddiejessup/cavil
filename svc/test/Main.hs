module Main where

import Cavil.Api.Decider
import Cavil.Api.Decider.Var
import Cavil.Hashing qualified as Hashing
import Cavil.Impl.Decider
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Mb
import Data.UUID qualified as UUID
import Protolude
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [ testCase "Single-decision field variant uniformity" testDecisionVarsUniform,
      testCase "Single-decision field variant sequence consistency" testDecisionVarsConsistent
    ]

fIdStream :: DecisionId -> [FieldId]
fIdStream decisionId = List.iterate' next (Hashing.nextFieldIdInChain (Left decisionId))
  where
    next :: FieldId -> FieldId
    next fId = Hashing.nextFieldIdInChain (Right fId)

countFreqs :: Ord k => [k] -> Map k Int
countFreqs xs = Map.fromListWith (+) (zip xs (repeat 1))

decisionIdFromUUIDText :: Text -> DecisionId
decisionIdFromUUIDText = DecisionId . Mb.fromJust . UUID.fromText

testDecisionVarsUniform :: Assertion
testDecisionVarsUniform =
  for_ decisionIds $ \decisionId ->
    for_ nrVarses $ \nrVars ->
      go decisionId nrVars
  where
    decisionIds =
      [ decisionIdFromUUIDText "c402dbd0-66e7-44a6-9ea1-8ca7f2f74b12",
        decisionIdFromUUIDText "23acad72-f5bd-4526-8c8e-d40d1ca9cb76",
        decisionIdFromUUIDText "4636c9b7-552a-420c-99a4-4779594751db"
      ]
    nrVarses :: [Int]
    nrVarses = [2, 3, 20]
    uniformTol :: Double
    uniformTol = 0.02
    sampleSize :: Int
    sampleSize = 1_000_000
    go :: DecisionId -> Int -> Assertion
    go sampleDecisionId sampleNrVariants =
      let varList = Mb.fromJust $ mkSimpleVariantList sampleNrVariants
          varSample =
            take
              sampleSize
              (pickVariant varList <$> fIdStream sampleDecisionId)
          expectedFreq = fromIntegral sampleSize / fromIntegral sampleNrVariants
          varFreqs = countFreqs varSample
          freqErr freq =
            abs (fromIntegral freq - expectedFreq) / expectedFreq
          varFreqFracErrs = Map.elems varFreqs <&> freqErr
          failMsg =
            "Biased decisions for decider ID '"
              <> show sampleDecisionId
              <> "' with '"
              <> show (sampleNrVariants)
              <> "' variants. Freqs: "
              <> show (Map.elems varFreqs)
              <> ". Errors: "
              <> show varFreqFracErrs
       in assertBool failMsg (maximum varFreqFracErrs < uniformTol)

testDecisionVarsConsistent :: Assertion
testDecisionVarsConsistent = do
  let varSample = unVariant . unVariantSelection <$> take 10 (pickVariant varList <$> fIdStream decisionId)
  assertEqual "Variant sequence inconsistent" expectedVarSample varSample
  where
    decisionId = decisionIdFromUUIDText "acc21409-aa45-4f98-9b19-8fc13812e3ad"
    varList = Mb.fromJust $ mkSimpleVariantList 2
    expectedVarSample = ["1", "0", "0", "1", "1", "1", "0", "1", "1", "1"]
