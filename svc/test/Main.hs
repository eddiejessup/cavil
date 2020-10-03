module Main where

import Cavil.Api.Ledger
import Cavil.Api.Ledger.Var
import Cavil.Hashing qualified as Hashing
import Cavil.Impl.Ledger
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
    [ testCase "Single-entry field variant uniformity" testFieldValSpecsUniform,
      testCase "Single-entry field variant sequence consistency" testFieldValSpecsConsistent
    ]

fIdStream :: EntryId -> [FieldId]
fIdStream entryId = List.iterate' next (Hashing.nextFieldIdInChain (Left entryId))
  where
    next :: FieldId -> FieldId
    next fId = Hashing.nextFieldIdInChain (Right fId)

countFreqs :: Ord k => [k] -> Map k Int
countFreqs xs = Map.fromListWith (+) (zip xs (repeat 1))

entryIdFromUUIDText :: Text -> EntryId
entryIdFromUUIDText = EntryId . Mb.fromJust . UUID.fromText

testFieldValSpecsUniform :: Assertion
testFieldValSpecsUniform =
  for_ entryIds $ \entryId ->
    for_ nrVarsList $ \nrVars ->
      go entryId nrVars
  where
    entryIds =
      [ entryIdFromUUIDText "c402dbd0-66e7-44a6-9ea1-8ca7f2f74b12",
        entryIdFromUUIDText "23acad72-f5bd-4526-8c8e-d40d1ca9cb76",
        entryIdFromUUIDText "4636c9b7-552a-420c-99a4-4779594751db"
      ]
    nrVarsList :: [Int]
    nrVarsList = [2, 3, 20]
    uniformTol :: Double
    uniformTol = 0.02
    sampleSize :: Int
    sampleSize = 1_000_000
    go :: EntryId -> Int -> Assertion
    go sampleEntryId sampleNrVariants =
      let varList = Mb.fromJust $ mkSimpleVariantList sampleNrVariants
          varSample =
            take
              sampleSize
              (pickVariant varList <$> fIdStream sampleEntryId)
          expectedFreq = fromIntegral sampleSize / fromIntegral sampleNrVariants
          varFreqs = countFreqs varSample
          freqErr freq =
            abs (fromIntegral freq - expectedFreq) / expectedFreq
          varFreqFracErrs = Map.elems varFreqs <&> freqErr
          failMsg =
            "Biased entries for ledger ID '"
              <> show sampleEntryId
              <> "' with '"
              <> show (sampleNrVariants)
              <> "' variants. Freqs: "
              <> show (Map.elems varFreqs)
              <> ". Errors: "
              <> show varFreqFracErrs
       in assertBool failMsg (maximum varFreqFracErrs < uniformTol)

testFieldValSpecsConsistent :: Assertion
testFieldValSpecsConsistent = do
  let varSample = unVariant . unVariantSelection <$> take 10 (pickVariant varList <$> fIdStream entryId)
  assertEqual "Variant sequence inconsistent" expectedVarSample varSample
  where
    entryId = entryIdFromUUIDText "acc21409-aa45-4f98-9b19-8fc13812e3ad"
    varList = Mb.fromJust $ mkSimpleVariantList 2
    expectedVarSample = ["1", "0", "0", "1", "1", "1", "0", "1", "1", "1"]
