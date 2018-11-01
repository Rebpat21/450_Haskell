{- Carrie's Candy Bowl, Code Skeleton (List Representation)
   H. Conrad Cunningham

1234567890123456789012345678901234567890123456789012345678901234567890

2018-10-21: Expanded from similar previous work

Notes:

-   In some cases, you may need to restrict the polymorphism to
    implement a function. Be careful not to restrict functions
    unnecessarily.

-   You may find Prelude functions such as concatMap, elem, filter,
    length, map, null, replicate, and span useful.

-   You may also find functions in the Data.List library
    useful -- e.g. sort, group, and (\\).

-}

module CandyBowl_list
  ( CandyBowl(..), newBowl, isEmpty, putIn, has, size, howMany, eqBowl, takeOut,
  )
where

-- Used in instructor solution
import Data.List ( sort, group, (\\) )

-- Candy bowl data representation
data CandyBowl a = Bowl [a] deriving Show

-- Remove comments from implemented type signatures below

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

-- Exercise #10
-- restock :: [(a,Int)] -> CandyBowl a

-- Exercise #11
-- combine :: CandyBowl a -> CandyBowl a -> CandyBowl a

-- Exercise #12
-- difference :: Eq a => CandyBowl a -> CandyBowl a -> CandyBowl a

-- Exercise #13
-- rename :: CandyBowl a -> (a -> b) -> CandyBowl b
