-- ask GHC for identity's type
identity x = x

-- identity is generic!
identity' :: a -> a
identity' x = x

-- works with ANY type
one :: Int
one = identity 1

hello :: String
hello = identity "hello"

identity'' :: a -> a
identity'' = identity identity


-- multiple arguments with currying
add :: Int -> Int -> Int
add x y = x + y

-- equivalent to
add' :: Int -> Int -> Int
add' = \x y -> x + y

-- and
add'' :: Int -> Int -> Int
add'' = \x -> \y -> x + y

-- partial application thanks to currying
addFive :: Int -> Int
addFive = add 5

twenty :: Int
twenty = addFive 15


-- operators are functions!
multiply :: Int -> Int -> Int
multiply = (*)

-- can be partially applied, too (sectioning)
multiplyBy10 :: Int -> Int
multiplyBy10 = * 10

-- or also
multiplyBy10' :: Int -> Int
multiplyBy10' = 10 *

fiveHundred :: Int
fiveHundred = multiplyBy10 50

-- makes for very convenient mapping
powersOfTwo :: [Int]
powersOfTwo = map (2 ^) [1..10]
