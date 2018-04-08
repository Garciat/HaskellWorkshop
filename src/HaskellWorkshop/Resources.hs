{-# LANGUAGE LambdaCase #-}

module HaskellWorkshop.Resources
  ( readItemFromFile
  , allItems
  ) where

import Control.Monad
import Data.Yaml              (decodeFileEither)
import System.FilePath        ((</>))
import System.Directory       (listDirectory)
import Text.Regex.Posix       ((=~))

import HaskellWorkshop.Types


readItemFromFile :: FilePath -> IO Item
readItemFromFile path =
  decodeFileEither path >>=
    \case
      Left err   -> fail (show err)
      Right item -> return item


allItems :: FilePath -> IO [Item]
allItems dirPath = do
  paths <- listDirectory dirPath
  let yamlFiles = map (dirPath </>) . filter (=~ "\\.yaml$") $ paths
  mapM readItemFromFile yamlFiles
