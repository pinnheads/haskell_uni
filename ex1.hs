{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use max" #-}
{-# HLINT ignore "Use min" #-}


-- Function to compute the maximum of two integers
maxi :: Int -> Int -> Int
maxi a b = if a > b then a else b

-- Function to compute the minimum of two integers
mini :: Int -> Int -> Int
mini a b = if a > b then b else a

-- Function to compute the maximum of three integers
max3 :: Int -> Int -> Int -> Int
max3 x y z = maxi (maxi x y) z

-- Function to compute the maximum of a 3-tuple
max3Tupled :: (Int, Int, Int) -> Int
max3Tupled (x, y, z) = max3 x y z

-- Function to compute the median of three integers
med :: Int -> Int -> Int -> Int
med x y z = x + y + z - mini x (mini y z) - maxi x (maxi y z)
