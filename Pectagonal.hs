{-# LANGUAGE BangPatterns #-}

-- inspired by
-- http://stackoverflow.com/questions/3208258/memoization-in-haskell
module Pectagonal where

import Control.Monad (replicateM)

data Tree a = Tree (Tree a) a (Tree a)
instance Functor Tree where
    fmap f (Tree l m r) = Tree (fmap f l) (f m) (fmap f r)

index :: Tree a -> Int -> a
index (Tree _ m _) 0 = m
index (Tree l _ r) n = case (n - 1) `divMod` 2 of
    (q,0) -> index l q
    (q,1) -> index r q

nats :: Tree Int
nats = go 0 1
    where
        go !n !s = Tree (go l s') n (go r s')
            where
                l = n + s
                r = l + s
                s' = s * 2

f_tree :: Tree Int
f_tree = fmap (mPectagonal pect) nats

pect :: Int -> Int
pect = index f_tree

mPectagonal :: (Int -> Int) -> Int -> Int
mPectagonal mf 0 = 0
mPectagonal mf 1 = 1
mPectagonal mf 2 = 5
mPectagonal mf n = let !overlap = overlappingSide n
                       !outerDots = 5 + 5*(n-2)
                       !prevPect = mf ( n - 1 )
                   in outerDots + prevPect - overlap

overlappingSide :: Int -> Int
overlappingSide 1 = 0
overlappingSide 2 = 0
overlappingSide m = 3 + 2 * (m - 2) - 2

main :: IO ()
main = do
    numOfCases <- fmap read getLine
    inputs <- replicateM numOfCases (fmap read getLine) :: IO [Int]
    mapM_ (print . pect) inputs
