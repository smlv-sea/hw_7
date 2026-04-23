module Main where

-- Бесконечный список частичных сумм гармонического ряда
Sums :: [Double]
Sums = scanl (+) 0 [1 / fromIntegral n | n <- [1..]]

-- Функция для получения n-го элемента (нумерация с 0)
getSum :: Int -> Double
getSum n = Sums !! n

-- Тесты
main :: IO ()
main = do
    putStrLn "Первые 10 частичных сумм гармонического ряда:"
    print $ take 10 Sums
    
    putStrLn "\nПроверка значений:"
    putStrLn $ "H1 = " ++ show (getSum 1)
    putStrLn $ "H2 = " ++ show (getSum 2)
    putStrLn $ "H3 = " ++ show (getSum 3)
    putStrLn $ "H10 = " ++ show (getSum 10)