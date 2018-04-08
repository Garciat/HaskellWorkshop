fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)


-- http://mathworld.wolfram.com/AckermannFunction.html
ackermann :: Int -> Int -> Int
ackermann 0 y = y + 1
ackermann x 0 = ackermann (x - 1) 1
ackermann x y = ackermann (x - 1) (ackermann x (y - 1))


factorial 0 = 1
factorial n = n * factorial (n - 1)

factorialHelper fact 0 = fact
factorialHelper fact n = factorialHelper (n * fact) (n - 1)

factorial' n = factorialHelper 1 n


data IntList
  = Empty
  | Link Int IntList

example = Link 1 (Link 2 (Link 3 Empty))


intListSum :: IntList -> Int
intListSum Empty = 0
intListSum (Link item theRest) = item + intListSum theRest

intListLength :: IntList -> Int
intListLength Empty = 0
intListLength (Link _ theRest) = 1 + intListLength theRest

mapIntList :: (Int -> Int) -> IntList -> IntList
mapIntList _ Empty = Empty
mapIntList f (Link item theRest) = Link (f item) (mapIntList f theRest)

exampleTimesTen = mapIntList (*10) example
