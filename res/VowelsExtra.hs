import Data.Map (Map, unionWith, empty, singleton)
import Data.Monoid


data Tally k v = TL (Map k v) deriving Show

instance (Ord k, Semigroup v) => Semigroup (Tally k v) where
  TL mapA <> TL mapB = TL (unionWith (<>) mapA mapB)

instance (Ord k, Monoid v) => Monoid (Tally k v) where
  mempty = TL empty

tally f items = mconcat [TL (singleton k (f k)) | k <- items]

countAllVowels4 :: String -> Tally Char (Sum Int)
countAllVowels4 = tally (\_ -> Sum 1)
