
greetWith greetWord = do
  putStr "Name: "
  name <- getLine
  putStr "LastName: "
  lastName <- getLine
  putStrLn (greetWord ++ ", " ++ name ++ " " ++ lastName ++ "!")

examples = do
  print (4 * 2)
  print [1..10]
  print "hola"

main = do
  greetWith "你好"
  examples
