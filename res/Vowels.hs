import Data.Map (Map, unionWith, empty, singleton)
import Data.Monoid


data VowelCount
  = VC Int Int Int Int Int
  deriving Show

emptyCount = VC 0 0 0 0 0

addVC (VC a b c d e) (VC p q r s t)
  = VC (a+p) (b+q) (c+r) (d+s) (e+t)

countVowel c =
  case c of
    'a' -> VC 1 0 0 0 0
    'e' -> VC 0 1 0 0 0
    'i' -> VC 0 0 1 0 0
    'o' -> VC 0 0 0 1 0
    'u' -> VC 0 0 0 0 1
    otherwise -> emptyCount

countAllVowels1 = foldl addVC emptyCount . map countVowel

---

instance Semigroup VowelCount where
  (<>) = addVC

instance Monoid VowelCount where
  mempty = emptyCount

countAllVowels2 = mconcat . map countVowel

---

data LetterCount = LC (Map Char Int) deriving Show

instance Semigroup LetterCount where
  LC mapA <> LC mapB = LC (unionWith (+) mapA mapB)

instance Monoid LetterCount where
  mempty = LC empty

letter c = LC (singleton c 1)

countAllVowels3 = mconcat . map letter
