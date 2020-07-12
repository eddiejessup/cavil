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
    [ testCase "Single-case decision uniformity" caseUniform
    ]

decTokenStream :: CaseLabel -> [DecisionToken]
decTokenStream caseLabel = List.iterate' next (Hashing.nextDecisionToken (Left caseLabel))
  where
    next :: DecisionToken -> DecisionToken
    next tok = Hashing.nextDecisionToken (Right tok)

decVarStream :: NrVariants -> [DecisionToken] -> [Variant]
decVarStream nrVars toks = Hashing.pickVariant nrVars <$> toks

countFreqs :: Ord k => [k] -> Map k Int
countFreqs xs = Map.fromListWith (+) (zip xs (repeat 1))

caseUniform :: Assertion
caseUniform =
  let tokStream :: [DecisionToken]
      tokStream = decTokenStream sampleCaseLabel
      varStream :: [Variant]
      varStream = decVarStream sampleNrVariants tokStream
      varSample :: [Variant]
      varSample = take sampleSize varStream
      expFreq :: Double
      expFreq = fromIntegral sampleSize / fromIntegral (nrVariantsInt sampleNrVariants)
      varFreqs :: Map Variant Int
      varFreqs = countFreqs varSample
      varFreqFracErrs :: [Double]
      varFreqFracErrs = Map.elems varFreqs <&> \freq -> abs (fromIntegral freq - expFreq) / expFreq
      maxAbsDiff :: Double
      maxAbsDiff = maximum varFreqFracErrs
   in assertBool "Biased decisions" (maxAbsDiff < uniformTol)
  where
    uniformTol :: Double
    uniformTol = 0.002
    sampleCaseLabel :: CaseLabel
    sampleCaseLabel = CaseLabel "foo"
    sampleNrVariants :: NrVariants
    sampleNrVariants = NrVariants 2
    sampleSize :: Int
    sampleSize = 1_000_000
