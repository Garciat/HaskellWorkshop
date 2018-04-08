import Control.Arrow
import Data.Char
import Data.List


hola = "hola"

hola' = 'h' : 'o' : 'l' : 'a' : []

holaMundo = hola ++ " mundo!"

colores = ["azul", "rojo", "negro"]

colores' = words "azul rojo negro"


---


shout input = map toUpper input ++ "!!!"


---


uniqueWords input =
  nub (sort (words input))

uniqueWords' input =
  (nub . sort . words) input

uniqueWords'' =
  nub . sort . words

uniqueWords''' =
  words >>> sort >>> nub

x |> f = f x

uniqueWords'''' input =
  input |> words |> sort |> nub


---


isort words =
  sortOn (\word -> map toLower word) words


---


oneAndTwo = (1, 2)

one = fst oneAndTwo

two = snd oneAndTwo


---


top5LongestWords input = error "TODO!"


---