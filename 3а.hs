-- Бесконечный список десятичных приближений дроби, числитель и знаменатель заданы
-- Каждый следующий элемент содержит на один знак больше после запятой


module Main where

decimalApprox :: Integer -> Integer -> [Double]
decimalApprox nom den = 
    [ fromIntegral (round (val * 10^n)) / 10^n | n <- [0..] ]
  where
    val = fromIntegral nom / fromIntegral den :: Double


showApprox :: Integer -> Integer -> Int -> IO ()
showApprox nom den n = 
    mapM_ print $ take n $ decimalApprox nom den
	
	
--ТЕСТЫ
testOneThird :: IO ()
testOneThird = do
    putStrLn "\n=== Для 2/3 ==="
    print $ take 6 $ decimalApprox 2 3
	

testHalf :: IO ()
testHalf = do
    putStrLn "\n=== Для 1/4 ==="
    print $ take 5 $ decimalApprox 1 4

main :: IO ()
main = do
    testOneThird
    testHalf