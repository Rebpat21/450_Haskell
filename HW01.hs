

module HW01 (
    prodSqSmall, xor, implies, ccArea, addTax, subTax, validDay, roman
    )
where

-- Number 2
prodSqSmall :: Double -> Double -> Double -> Double
prodSqSmall a b c
  | a < b = prodSqSmall b a c
  | c < a = prodSqSmall b c a
  | otherwise = (a^2) * (b^2)

-- Number 3
xor :: Bool -> Bool -> Bool
xor a b
  | a == b = False
  | otherwise = True

--Number 4
implies :: Bool -> Bool -> Bool
implies p q
  | (p == True) && (q == False) = False
  | otherwise = True

--Number 7
ccArea :: Double -> Double -> Double
ccArea x y
  | x < 0 || y < 0 = error ""
  | y > x = (pi * (y/2)^2 ) - ( pi * (x/2)^2)
  | otherwise = (pi * (x/2)^2 ) - (pi * (y/2)^2)


--Number 9
addTax :: Double -> Double -> Double
addTax c p =
  (c + (c * (p/100.0)))

subTax :: Double -> Double -> Double
subTax c p =
  (c / (1 + (p / 100)))

--Number 11
validDay :: (Int, Int, Int) -> Bool
validDay (m,d,y)
  | ((m < 0) || (d < 0) || (m > 12) || (d > 31) || (y < 0)) = False
  -- Checks for faulty data. Such as negatives or 0's
  | (((m == 1) || (m == 3) || (m == 5) || (m == 7) || (m == 8) || (m == 10) || (m == 12)) && (d <= 31)) = True
  --
  | ((m == 2) && (d == 29) && ((y `mod` 400 == 0) && (y `mod` 4 == 0) && (y `mod` 100 == 0))) = True
  | (((m == 4) || (m == 6) || (m == 9) || (m == 11)) && (d <= 30)) = True
  | otherwise = False

--Number 12
roman :: Int -> String
roman q
  | q > 0 = "This one doesn't work, sorry Kyle!"
  | q < 0 = "Nope"
  | otherwise = "Still Doesn't work."
