
-- Векторное пространство с операциями +,* на константу и взятие противоположного элемента

module VectorSpace where

-- Тип данных "вектор" - список чисел
data Vector = Vector [Double]
    deriving (Show, Eq)

-- 1. Сложение векторов
plus :: Vector -> Vector -> Vector
plus (Vector v1) (Vector v2) = Vector (zipWith (+) v1 v2)

-- 2. Умножение на константу 
scale :: Double -> Vector -> Vector
scale s (Vector v) = Vector (map (* s) v)

-- 3. Взятие противоположного элемента
negateVec :: Vector -> Vector
negateVec v = scale (-1) v

-- Тесты
main :: IO ()
main = do
    let v1 = Vector [1, 2, 3]
    let v2 = Vector [4, 5, 6]
    
    putStrLn "Вектор v1:"
    print v1
    putStrLn "Вектор v2:"
    print v2
    putStrLn "Сложение v1 + v2:"
    print (plus v1 v2)
    putStrLn "Умножение v1 на 2:"
    print (scale 2 v1)
    putStrLn "Противоположный вектор для v1:"
    print (negateVec v1)