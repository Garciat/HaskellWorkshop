import Data.List


listA = [1, 2, 3, 4]

listB = []

listC = 0 : listA

listC' = 0 : 1 : 2 : 3 : 4 : []

listC'' = [0..4]

listD = 1 : listD

listE = take 10 listD

isEven x = mod x 2 == 0

listF = filter isEven listC

listG = [x * x | x <- listC, x > 2]

naturals = [0..]

squares = [x * x | x <- naturals]

listH = [x * y | x <- listC, y <- listA]

listI = listA ++ listA

unordered = [4, 0, 3, 1, 2]

listJ = sort unordered

listJRev = reverse listJ

listJRev' = sortOn (\x -> -x) unordered

factorial n = product [1..n]

listK = map factorial listJ

listL = map (\n -> product [1..n]) listJ
