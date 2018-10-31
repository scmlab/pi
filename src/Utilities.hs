module Utilities where

import Control.Arrow ((***))
import Control.Monad

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

nodup :: Eq a => [a] -> Bool
nodup [] = True
nodup (x:xs) = not (x `elem` xs) && nodup xs

fork3 :: (x -> a) -> (y -> b) -> (z -> c) -> (x, y, z) -> (a, b, c)
fork3 f g h (x,y,z) = (f x, g y, h z)
