{-# LANGUAGE TypeApplications #-}

module Cavil.Event.Project where

import Cavil.Api.Case
import Cavil.Event.Event
import Data.Generics.Product.Typed
import Protolude hiding (to, (%))

caseLabelsFromEvents :: [CaseEvent] -> [CaseLabel]
caseLabelsFromEvents = foldl' go []
  where
    go caseLabels = \case
      CaseCreated ccEvt -> caseLabels <> [getTyped @CaseLabel ccEvt]
      DecisionMade _ -> caseLabels
      DecisionInvalidated _ -> caseLabels
