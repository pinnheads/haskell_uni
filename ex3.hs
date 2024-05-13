-- head' function
head' :: [a] -> a
head' (x:_) = x

-- tail' function
tail' :: [a] -> [a]
tail' (_:xs) = xs

-- last' function
last' :: [a] -> a
last' [x] = x
last' (_:xs) = last' xs

-- length' function
length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

-- and' function
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

-- init' function
init' :: [a] -> [a]
init' [_] = []
init' (x:xs) = x : init' xs

-- (+++) function
(+++) :: [a] -> [a] -> [a]
[] +++ ys = ys
(x:xs) +++ ys = x : (xs +++ ys)

-- zip' function
zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

-- reverse' function
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs +++ [x]