-- Stack operations
push :: Integer -> [Integer] -> [Integer]
push n stack = n : stack

pop :: [Integer] -> [Integer]
pop [] = [0]
pop (_:xs) = xs

dup :: [Integer] -> [Integer]
dup [] = [0]
dup (x:xs) = x : x : xs

add :: [Integer] -> [Integer]
add [] = [0]
add [x] = [x]
add (x:y:xs) = (x + y) : xs

subtract' :: [Integer] -> [Integer]
subtract' [] = [0]
subtract' [x] = [-x]
subtract' (x:y:xs) = (y - x) : xs

multiply :: [Integer] -> [Integer]
multiply [] = [0]
multiply [x] = [x]
multiply (x:y:xs) = (x * y) : xs

neg :: [Integer] -> [Integer]
neg [] = [0]
neg (x:xs) = (-x) : xs

-- readCommand function
readCommand :: String -> [Integer] -> [Integer]
readCommand command stack
  | command == "pop" = pop stack
  | command == "dup" = dup stack
  | command == "add" = add stack
  | command == "subtract" = subtract' stack
  | command == "multiply" = multiply stack
  | command == "neg" = neg stack
  | "push " `isPrefixOf` command = push (read (drop 5 command) :: Integer) stack
  | otherwise = stack  -- unrecognized operation, leave the stack unchanged