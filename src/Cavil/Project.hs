{-# LANGUAGE TypeApplications #-}

module Cavil.Project where

import Cavil.Api
import Cavil.Event
import Data.Generics.Product.Typed
import Protolude hiding ((%), to)

caseLabelsFromEvents :: [CaseEvent] -> [CaseLabel]
caseLabelsFromEvents = foldl' go []
  where
    go caseLabels = \case
      CaseCreated ccEvt -> caseLabels <> [getTyped @CaseLabel ccEvt]
      DecisionMade _ -> caseLabels
      DecisionInvalidated _ -> caseLabels
