{-# LANGUAGE TypeApplications #-}

module Cavil.Project where

import Cavil.Api
import Cavil.Event
import Data.Generics.Product.Typed
import Optics
import Protolude hiding ((%), to)

caseLabelsFromEvents :: [CaseEvent] -> [CaseLabel]
caseLabelsFromEvents = foldl' go []
  where
    go caseLabels = \case
      CaseCreated ccEvt -> caseLabels <> [ccEvt ^. typed @CaseLabel]
      DecisionMade _ -> caseLabels
      DecisionInvalidated _ -> caseLabels
