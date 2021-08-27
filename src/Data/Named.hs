module Data.Named where

import Data.Maybe ( fromMaybe )
import Data.List ( find )
type Name = String

class Named a where
    getName :: a -> Name

matchesN :: Named a => a -> a -> Bool
matchesN a a' = getName a == getName a'

hasN :: Named a => a -> [a] -> Bool
hasN n ns = getName n `elem` fmap getName ns

filterN :: Named a => a -> [a] -> [a]
filterN n = filter $ not . matchesN n

findN :: (Named a, Monoid a) => a -> [a] -> a
findN n ns = fromMaybe mempty match
             where match = find (matchesN n) ns

infix 6 <:> 
(<:>) :: (Named a, Monoid a) => a -> [a] -> [a]
(<:>) n ns = if not $ hasN n ns then ns ++ [n] else n <> findN n ns : filterN n ns

infix 6 <..>
(<..>) :: (Named a, Monoid a) => [a] -> [a] -> [a]
(<..>) [] [] = []
(<..>) [] ns' = ns'
(<..>) ns [] = ns
(<..>) ns (n: ns') 
      | hasN n ns =  (n <:> ns) <..> ns'
      | otherwise  = (n  : ns) <..> ns'
