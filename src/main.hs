-- Write printAMessage here
printAMessage :: Show a => a -> IO ()
printAMessage x = print x

-- Write division here
division :: Double -> Double -> Maybe Double
division x 0 = Nothing
division x y = Just (x / y)

-- Write factorial here
factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial n = n * (factorial (n - 1))

-- Write factList here
factList :: Int -> [Int]
factList n = map factorial [1..n]

-- Write merge here
merge :: [Int] -> [Int] -> [Int]
merge x [] = x
merge [] y = y
merge (x : xs) (y : ys) 
    | x < y = x : merge xs (y : ys)
    | otherwise = y : merge (x : xs) ys

-- main = print "Test" -- Replace this with your testing code
main = do
    putStr "\n"
    printAMessage "Hello World!"
    putStr "\n"
    let z = division 1 2
    let w = division 1 0
    let g = division 6 2
    putStr "Testing division: \n"
    print z
    print w
    print g
    putStr "\n"
    putStr "Testing factorial: \n"
    let a = factorial 1
    let b = factorial 7
    print a
    print b
    putStr "\n"
    putStr "Testing factList: \n"
    let testList = factList 5
    print testList
    putStr "\n"
    putStr "Testing merge: \n"
    let merged = merge [1, 3, 6] [2, 4, 5, 6, 7]
    print merged
    putStr "\n"
