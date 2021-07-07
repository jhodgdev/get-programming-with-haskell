-- Q21.2 --

fib :: Int -> Integer
fib = fastFib 0 1 

fastFib n1 n2 0 = n1
fastFib n1 n2 counter = fastFib n2 (n1 + n2) (counter - 1)

main :: IO ()
main = do
  putStrLn "input: "
  n <- getLine
  let result = fib $ read n
  print result
