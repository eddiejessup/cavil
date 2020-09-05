{-# LANGUAGE NumericUnderscores #-}

module Main where

import Cavil.Api.Decider
import qualified Cavil.Hashing as Hashing
import Cavil.Impl.Decider
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Mb
import qualified Data.UUID as UUID
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
    [ testCase "Single-decider decision uniformity" testDeciderVarsUniform,
      testCase "Single-decider variant sequence consistency" testDeciderVarsConsistent
    ]

dIdStream :: DeciderId -> [DecisionId]
dIdStream deciderId = List.iterate' next (Hashing.nextDecisionId (Left deciderId))
  where
    next :: DecisionId -> DecisionId
    next dId = Hashing.nextDecisionId (Right dId)

countFreqs :: Ord k => [k] -> Map k Int
countFreqs xs = Map.fromListWith (+) (zip xs (repeat 1))

deciderIdFromUUIDText :: Text -> DeciderId
deciderIdFromUUIDText = DeciderId . Mb.fromJust . UUID.fromText

testDeciderVarsUniform :: Assertion
testDeciderVarsUniform =
  for_ deciderIds $ \deciderId ->
    for_ nrVarses $ \nrVars ->
      go deciderId nrVars
  where
    deciderIds =
      [ deciderIdFromUUIDText "c402dbd0-66e7-44a6-9ea1-8ca7f2f74b12",
        deciderIdFromUUIDText "23acad72-f5bd-4526-8c8e-d40d1ca9cb76",
        deciderIdFromUUIDText "4636c9b7-552a-420c-99a4-4779594751db"
      ]
    nrVarses :: [NrVariants]
    nrVarses = [NrVariants 2, NrVariants 3, NrVariants 20]
    uniformTol :: Double
    uniformTol = 0.02
    sampleSize :: Int
    sampleSize = 1_000_000
    go :: DeciderId -> NrVariants -> Assertion
    go sampleDeciderId sampleNrVariants =
      let varSample =
            take
              sampleSize
              (pickVariant sampleNrVariants <$> dIdStream sampleDeciderId)
          expectedFreq = fromIntegral sampleSize / fromIntegral (unNrVariants sampleNrVariants)
          varFreqs = countFreqs varSample
          freqErr freq =
            abs (fromIntegral freq - expectedFreq) / expectedFreq
          varFreqFracErrs = Map.elems varFreqs <&> freqErr
          failMsg =
            "Biased decisions for decider ID '"
              <> show sampleDeciderId
              <> "' with '"
              <> show (unNrVariants sampleNrVariants)
              <> "' variants. Freqs: "
              <> show (Map.elems varFreqs)
              <> ". Errors: "
              <> show varFreqFracErrs
       in assertBool failMsg (maximum varFreqFracErrs < uniformTol)

testDeciderVarsConsistent :: Assertion
testDeciderVarsConsistent = do
  let varSample = take 10 (pickVariant nrVars <$> dIdStream deciderId)
  assertEqual "Variant sequence inconsistent" varSample expectedVarSample
  where
    deciderId = deciderIdFromUUIDText "acc21409-aa45-4f98-9b19-8fc13812e3ad"
    nrVars = NrVariants 2
    expectedVarSample = Variant <$> [1, 1, 1, 0, 1, 1, 0, 0, 0, 1]
