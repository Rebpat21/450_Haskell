
module CandyBowl_list
  ( CandyBowl(..), newBowl, isEmpty, putIn, has, size, howMany, eqBowl, takeOut, combine
  )
where

-- Used in instructor solution
import Data.List ( sort, group, (\\) )
-- import Data.Map as Map

-- Candy bowl data representation
data CandyBowl a = Bowl [a] deriving Show

-- Exercise #1
newBowl :: CandyBowl a
newBowl = Bowl[]

-- Exercise #2
isEmpty :: CandyBowl a -> Bool
isEmpty (Bowl candy)
  | length(candy) == 0 = True
  | otherwise = False

-- Exercise #3
putIn :: CandyBowl a -> a -> CandyBowl a
putIn (Bowl bowl) candy = Bowl(bowl ++ [candy])

-- Exercise #4
-- Resource used: https://www.programming-idioms.org/idiom/12/check-if-list-contains-a-value
has :: Eq a => CandyBowl a -> a -> Bool
has (Bowl candy) a = a `elem` candy

-- Exercise #5
size :: CandyBowl a -> Int
size (Bowl candy) = length(candy)

-- Exercise #6
-- Resource used: https://codereview.stackexchange.com/questions/139587/count-occurrences-of-an-element-in-a-list
howMany :: Eq a => CandyBowl a -> a -> Int
-- howMany (Bowl x) _ = 0
howMany (Bowl candy) x = sum $ map (\a -> 1) $ filter (== x) candy

-- Exercise #7
takeOut :: (Eq a,Eq a) => CandyBowl a -> a -> Maybe (CandyBowl a)
takeOut (Bowl []) xs = Nothing
takeOut (Bowl candy@(x:a)) b
  | (has(Bowl candy) b) == False = Nothing
  | otherwise = Just (Bowl(iterateP candy b))

iterateP :: Eq a => [a] -> a -> [a]
iterateP [] a = []
iterateP q@(a:b) p
  | a == p = b
  | otherwise = a:(iterateP b p)

-- Exercise #8
-- Discussed with Doron Reisenger
eqBowl :: Ord a => CandyBowl a -> CandyBowl a -> Bool
eqBowl (Bowl []) (Bowl []) = True
eqBowl (Bowl q@(a:b)) (Bowl []) = False
eqBowl (Bowl []) (Bowl c) = False
eqBowl (Bowl q@(a:b)) (Bowl c) =
  case (takeOut (Bowl c) a) of
    Nothing -> False
    Just (Bowl d) -> eqBowl (Bowl b) (Bowl d)

-- Exercise #9
-- inventory :: Ord a => CandyBowl a -> [(a,Int)]
-- inventory (Bowl candy) = low
--   where stuff = (group . sort) candy
--         thing = map length stuff
--         low = has ((tempFunct . concat) stuff) thing
--
-- tempFunct :: Ord a => [a] -> [a]
-- tempFunct (x:y:xs)
--   | x == y = tempFunct (y:xs)
--   | x /= y : x : tempFunct (y:xs)
--   tempFunct xs : xs

-- Exercise #10
-- restock :: [(a,Int)] -> CandyBowl a

-- Exercise #11
-- https://stackoverflow.com/questions/3938438/merging-two-lists-in-haskell
combine :: CandyBowl a -> CandyBowl a -> CandyBowl a
combine (Bowl []) (Bowl ys) = ys
combine (x:xs) ys = x:merge ys xs
-- combine (Bowl xs) (Bowl []) = xs
-- combine (Bowl []) (Bowl y:ys) = ys
-- combine (Bowl (x:xs)) (Bowl (y:ys)) = x : y : combine (Bowl xs) (Bowl ys)

-- Exercise #12
-- difference :: Eq a => CandyBowl a -> CandyBowl a -> CandyBowl a

-- Exercise #13
-- rename :: CandyBowl a -> (a -> b) -> CandyBowl b
