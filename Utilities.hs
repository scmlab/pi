module Utilities where

-- finite map related stuffs

type FMap a b = [(a,b)]

domain :: FMap a b -> [a]
domain = map fst

isEmpty :: FMap a b -> Bool
isEmpty = null

inDom :: Eq a => a -> FMap a b -> Bool
inDom x xs = x `elem` domain xs

fMapUpdate i f [] = []
fMapUpdate i f ((j,x):xs)
   | i == j    = (i,f x) : xs
   | otherwise = (j,x) : fMapUpdate i f xs

rmEntry i [] = []
rmEntry i ((j,x):xs) | i == j    = rmEntry i xs
                     | otherwise = (j,x) : rmEntry i xs
