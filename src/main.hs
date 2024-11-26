-- Write printAMessage here
printAMessage :: Show a => a -> IO ()
printAMessage a = putStrLn (show a)

-- Write division here
division :: Double -> Double -> Maybe Double
division x y
    | y == 0    = Nothing
    | otherwise = Just (x / y) 

-- Write factorial here
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial(n-1)

-- Write factList here
factList :: Int -> [Int]
factList n = [factorial x | x <- [1..n]]

-- Write merge here
merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x <= y    = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

main = do
    printAMessage "Hello, world!"
    putStrLn (show (division 1 2))
    putStrLn (show (factorial 5))
    putStrLn (show (factList 5))
    putStrLn (show (merge [1, 3, 6] [2, 4, 5, 6, 7]))