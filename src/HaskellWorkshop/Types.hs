{-# LANGUAGE DeriveGeneric #-}

module HaskellWorkshop.Types
  ( HaskellFeature(..)
  , Item(..)
  , allFeatures
  ) where

import Data.Yaml
import GHC.Generics


data HaskellFeature
  = BasicTypes
  | BasicSyntax
  | BasicData
  | DoSyntax
  | Main
  | LanguageExtensions
  | Lazyness
  | Purity
  | TypeClasses
  | PatternMatching
  | AdvancedTypes
  | Libraries
  | PropertyTesting
  | PartialFunctions
  | Abstraction
  | Operators
  | Functions
  | Records
  | Recursion
  | GeneralProgramming
  | Sectioning
  | StringsAsLists
  | MutableState
  | Currying
  deriving (Eq, Ord, Enum, Bounded, Show, Generic)

instance FromJSON HaskellFeature

instance ToJSON HaskellFeature


data Item
  = Item
  { itemTitle :: String
  , itemFilename :: String
  , itemDescription :: String
  , itemDependencies :: [HaskellFeature]
  , itemIntroductions :: [HaskellFeature]
  }
  deriving (Eq, Show, Generic)

instance FromJSON Item

instance ToJSON Item


allFeatures :: [HaskellFeature]
allFeatures = [minBound..maxBound]
