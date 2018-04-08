module HaskellWorkshop.Lecture
  ( presentationOrder
  , allIntroductions
  , featuresNotIntroduced
  ) where

import Data.Graph (stronglyConnComp, flattenSCCs)
import Data.List ((\\), intercalate, nub, sort)
import Data.Maybe (fromJust)

import HaskellWorkshop.Types
import HaskellWorkshop.Util


presentationOrder :: [Item] -> [Item]
presentationOrder items =
  allNodes
  |> stronglyConnComp
  |> flattenSCCs
  |> nub
  |> tail  -- gets rid of the "root" node, which naturally rises to the top
  |> map fromJust
  where
    rootNode = (Nothing, Nothing, [])

    allNodes = rootNode : concatMap toNodes items

    toNodes item =
      -- `Maybe` is used to keep the graph connected
      -- with a global dependency to a "root" node,
      -- which does not change the overall topography.
      map (\ft -> (Just item, Just ft, Nothing : fmap Just outwards)) local
      where
        local = itemIntroductions item
        outwards = itemDependencies item


allIntroductions :: [Item] -> [HaskellFeature]
allIntroductions items =
  concatMap itemIntroductions items


featuresNotIntroduced :: [Item] -> [HaskellFeature]
featuresNotIntroduced items =
  allFeatures \\ allIntroductions items
