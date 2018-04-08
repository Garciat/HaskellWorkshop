import Data.Char
import Data.Maybe


safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)


readInt :: String -> Maybe Int
readInt s
  | all isDigit s = Just (read s)
  | otherwise     = Nothing


divideNums :: String -> String -> Maybe Int
divideNums numA numB = do
  a <- readInt numA
  b <- readInt numB
  c <- safeDiv a b
  return c

x // y = divideNums x y

main = do
  print (divideNums "123" "32")
  print (divideNums "123" "0")
  print (divideNums "hola" "10")
  print (divideNums "321" "chau")

  print ("100" // "20")
