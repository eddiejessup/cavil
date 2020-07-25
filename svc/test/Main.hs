{-# LANGUAGE NumericUnderscores #-}

module Main where

import Cavil.Api.Case
import qualified Cavil.Hashing as Hashing
import Cavil.Impl.Case
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
    [ testCase "Single-case decision uniformity" testCaseVarsUniform
    ]

dIdStream :: CaseId -> [DecisionId]
dIdStream caseId = List.iterate' next (Hashing.nextDecisionId (Left caseId))
  where
    next :: DecisionId -> DecisionId
    next dId = Hashing.nextDecisionId (Right dId)

countFreqs :: Ord k => [k] -> Map k Int
countFreqs xs = Map.fromListWith (+) (zip xs (repeat 1))

caseIdFromUUIDText :: Text -> CaseId
caseIdFromUUIDText = CaseId . Mb.fromJust . UUID.fromText

testCaseVarsUniform :: Assertion
testCaseVarsUniform =
  for_ caseIds $ \caseId ->
    for_ nrVarses $ \nrVars ->
      go caseId nrVars
  where
    caseIds =
      [ caseIdFromUUIDText "c402dbd0-66e7-44a6-9ea1-8ca7f2f74b12",
        caseIdFromUUIDText "23acad72-f5bd-4526-8c8e-d40d1ca9cb76",
        caseIdFromUUIDText "4636c9b7-552a-420c-99a4-4779594751db"
      ]
    nrVarses :: [NrVariants]
    nrVarses = [NrVariants 2, NrVariants 3, NrVariants 20]
    uniformTol :: Double
    uniformTol = 0.02
    sampleSize :: Int
    sampleSize = 1_000_000
    go :: CaseId -> NrVariants -> Assertion
    go sampleCaseId sampleNrVariants =
      let varSample =
            take
              sampleSize
              (pickVariant sampleNrVariants <$> dIdStream sampleCaseId)
          expectedFreq = fromIntegral sampleSize / fromIntegral (unNrVariants sampleNrVariants)
          varFreqs = countFreqs varSample
          freqErr freq =
            abs (fromIntegral freq - expectedFreq) / expectedFreq
          varFreqFracErrs = Map.elems varFreqs <&> freqErr
          failMsg =
            "Biased decisions for case ID '"
              <> show sampleCaseId
              <> "' with '"
              <> show (unNrVariants sampleNrVariants)
              <> "' variants. Freqs: "
              <> show (Map.elems varFreqs)
              <> ". Errors: "
              <> show varFreqFracErrs
       in assertBool failMsg (maximum varFreqFracErrs < uniformTol)
