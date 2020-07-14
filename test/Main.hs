{-# LANGUAGE NumericUnderscores #-}

module Main where

import Cavil.Api
import qualified Cavil.Hashing as Hashing
import qualified Data.List as List
import qualified Data.Map.Strict as Map
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

tokenStream :: CaseLabel -> [DecisionToken]
tokenStream caseLabel = List.iterate' next (Hashing.nextDecisionToken (Left caseLabel))
  where
    next :: DecisionToken -> DecisionToken
    next tok = Hashing.nextDecisionToken (Right tok)

countFreqs :: Ord k => [k] -> Map k Int
countFreqs xs = Map.fromListWith (+) (zip xs (repeat 1))

testCaseVarsUniform :: Assertion
testCaseVarsUniform =
  for_ caseLabels $ \caseLabel ->
    for_ nrVarses $ \nrVars ->
      go caseLabel nrVars
  where
    caseLabels :: [CaseLabel]
    caseLabels = [CaseLabel "foo", CaseLabel "", CaseLabel "coffeeeeeeeeeeeeeeeeeeee"]

    nrVarses :: [NrVariants]
    nrVarses = [NrVariants 2, NrVariants 3, NrVariants 20]

    uniformTol :: Double
    uniformTol = 0.02

    sampleSize :: Int
    sampleSize = 1_000_000

    go :: CaseLabel -> NrVariants -> Assertion
    go sampleCaseLabel sampleNrVariants =
      let varSample = take sampleSize
            (Hashing.pickVariant sampleNrVariants <$> tokenStream sampleCaseLabel)

          expectedFreq = fromIntegral sampleSize / fromIntegral (nrVariantsInt sampleNrVariants)

          varFreqs = countFreqs varSample

          freqErr freq =
            abs (fromIntegral freq - expectedFreq) / expectedFreq

          varFreqFracErrs = Map.elems varFreqs <&> freqErr

          failMsg =
            "Biased decisions for label '"
            <> caseLabelText sampleCaseLabel
            <> "' with '"
            <> show (nrVariantsInt sampleNrVariants)
            <> "' variants. Freqs: "
            <> show (Map.elems varFreqs)
            <> ". Errors: "
            <> show varFreqFracErrs
       in
          assertBool (toS failMsg) (maximum varFreqFracErrs < uniformTol)
