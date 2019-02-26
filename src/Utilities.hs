module Utilities where

import Control.Arrow ((***))
import Control.Monad


nodup :: Eq a => [a] -> Bool
nodup [] = True
nodup (x:xs) = not (x `elem` xs) && nodup xs

nubcons :: Eq a => a -> [a] -> [a]
nubcons x xs | x `elem` xs = xs
             | otherwise   = x : xs

nubapp :: Eq a => [a] -> [a] -> [a]
nubapp [] ys = ys
nubapp (x:xs) ys = nubcons x (nubapp xs ys)

nubconcat :: Eq a => [[a]] -> [a]
nubconcat = foldr nubapp []

setminus :: Eq a => [a] -> [a] -> [a]
setminus xs []     = xs
setminus xs (y:ys) = setminus (filter (not . (y==)) xs) ys

subsetEq :: Eq a => [a] -> [a] -> Bool
[] `subsetEq` _ = True
(x:xs) `subsetEq` ys = x `elem` ys && xs `subsetEq` ys

setEq :: Eq a => [a] -> [a] -> Bool
setEq xs ys = xs `subsetEq` ys && ys `subsetEq` xs

-- finite map related stuffs

type FMap a b = [(a,b)]

domain :: FMap a b -> [a]
domain = map fst

isEmpty :: FMap a b -> Bool
isEmpty = null

inDom :: Eq a => a -> FMap a b -> Bool
inDom x xs = x `elem` domain xs

fMapUpdate :: Eq a => a -> b -> (b -> b) ->
              FMap a b -> FMap a b
fMapUpdate i e _ [] = [(i,e)]
fMapUpdate i e f ((j,x):xs)
   | i == j    = (i,f x) : xs
   | otherwise = (j,x) : fMapUpdate i e f xs

fMapPut :: Eq a => a -> b -> FMap a b -> FMap a b
fMapPut k v = fMapUpdate k v (const v)

selectAll :: Eq a => a -> FMap a b -> ([b], FMap a b)
selectAll _ [] = ([],[])
selectAll i ((j,x):xs)
  | i == j    = ((x:) *** id) (selectAll i xs)
  | otherwise = (id *** ((j,x):)) (selectAll i xs)

selectByKey :: (MonadPlus m, Eq a) =>
    a -> FMap a b -> m (b, FMap a b)
selectByKey _ [] = mzero
selectByKey i ((j,x):xs)
  | i == j    =
     return (x, xs) `mplus`
     ((id *** ((j,x):)) <$> (selectByKey i xs))
  | otherwise = (id *** ((j,x):)) <$> (selectByKey i xs)

rmEntry :: Eq a => a -> [(a, b)] -> [(a, b)]
rmEntry _ [] = []
rmEntry i ((j,x):xs) | i == j    = rmEntry i xs
                     | otherwise = (j,x) : rmEntry i xs

rmEntries [] xs = xs
rmEntries (i:is) xs = rmEntries is (rmEntry i xs)

fMapSubsetBy :: Eq a => (b -> b -> Bool) ->
                [(a,b)] -> [(a,b)] -> Bool
fMapSubsetBy f [] ys = True
fMapSubsetBy f ((x,z):xs) ys =
  case lookup x ys of
    Nothing -> False
    Just z' -> if f z z' then fMapSubsetBy f xs ys
                         else False

fMapEqBy :: Eq a => (b -> b -> Bool) ->
                [(a,b)] -> [(a,b)] -> Bool
fMapEqBy f xs ys = fMapSubsetBy f xs ys &&
                   fMapSubsetBy f ys xs

-- misc.

fork3 :: (x -> a) -> (y -> b) -> (z -> c) -> (x, y, z) -> (a, b, c)
fork3 f g h (x,y,z) = (f x, g y, h z)

xor :: Bool -> Bool -> Bool
xor a b = not (a == b)
