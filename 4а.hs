--число, разложенное на простые множители. НОК и НОД


module Main where

-- Разложение числа на простые множители
factorize :: Integer -> [(Integer, Integer)]
factorize 1 = []
factorize n = factorize' n 2
  where
    factorize' 1 _ = []
    factorize' m p
      | p * p > m = [(m, 1)]
      | m `mod` p == 0 = (p, count) : factorize' (m `div` (p ^ count)) (p + 1)
      | otherwise = factorize' m (p + 1)
      where
        count = fromIntegral $ length $ takeWhile (\x -> x `mod` p == 0) (iterate (`div` p) m)

-- НОД 
myGCD :: Integer -> Integer -> Integer
myGCD a b = product [p ^ min ea eb | (p, ea) <- fa, let eb = getExp p fb, eb > 0]
  where
    fa = factorize a
    fb = factorize b
    
    getExp _ [] = 0
    getExp p ((p', e):rest)
      | p == p' = e
      | otherwise = getExp p rest

-- НОК 
myLCM :: Integer -> Integer -> Integer
myLCM a b = product [p ^ max ea eb | (p, ea) <- fa, let eb = getExp p fb]
  where
    fa = factorize a
    fb = factorize b
    
    getExp _ [] = 0
    getExp p ((p', e):rest)
      | p == p' = e
      | otherwise = getExp p rest

-- тест
main :: IO ()
main = do
    putStrLn "=== НОД и НОК ===\n"
    
    let pairs = [(12, 18), (24, 36), (7, 13), (16, 24), (100, 25)]
    
    putStrLn "Результаты:"
    mapM_ (\(a, b) -> do
        putStrLn $ "НОД(" ++ show a ++ ", " ++ show b ++ ") = " ++ show (myGCD a b)
        putStrLn $ "НОК(" ++ show a ++ ", " ++ show b ++ ") = " ++ show (myLCM a b)
        putStrLn "") pairs