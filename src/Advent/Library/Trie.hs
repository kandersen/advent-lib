{-# LANGUAGE RecordWildCards #-}
module Advent.Library.Trie(
  Trie(value, children),
  empty,
  singleton,
  insert,
  insertCPS,
  insert'',
  insert''',
  insertWithHandler,
  member,

  ) where

import Data.Map (Map)
import qualified Data.Map as Map

data Trie c a = Trie { 
    value :: a,
    children :: Map c (Trie c a)
  }

empty :: Trie c (Maybe a)
empty = Trie Nothing Map.empty

member :: Ord c => [c] -> Trie c Bool -> Bool
member [] Trie { .. } = value
member (x:xs) Trie { .. } = 
  let mv = Map.lookup x children in 
  case mv of
    Nothing -> False
    Just cs -> member xs cs

singleton :: a -> Trie c a
singleton a = Trie a Map.empty

insert :: Ord c => [c] -> Trie c Bool -> Trie c Bool
insert [] (Trie _ t) = Trie True t
insert (x:xs) Trie { .. } = 
  let mv = Map.lookup x children in
  case mv of
    Nothing -> Trie value (Map.insert x (insert xs (singleton False)) children)
    Just t -> Trie value (Map.insert x (insert xs t) children)

insertCPS :: Ord c => [c]-> Trie c Bool -> (Trie c Bool -> a) -> a
insertCPS [] (Trie _ t) k = k $ Trie True t
insertCPS (x:xs) Trie{..} k =
  let mv = Map.lookup x children in
  case mv of
    Nothing -> insertCPS xs (singleton False) $ \newChild ->
                k (Trie value (Map.insert x newChild children))
    Just t -> insertCPS xs t $ \newChild ->
                k (Trie value (Map.insert x newChild children))

insertWithHandler :: Ord c => 
  (a -> ret -> (a -> ret) -> ret) -> 
  a -> 
  [c] -> 
  Trie c a -> 
  ret -> 
  (Trie c a -> ret) 
  -> ret
insertWithHandler h _ [] Trie { .. } fk sk = h value fk (sk . flip Trie children)
insertWithHandler h def (x:xs) Trie { .. } fk sk = 
  let mv = Map.lookup x children in
  case mv of 
    Nothing -> insertWithHandler h def xs (singleton def) fk $ \newChild ->
                 sk (Trie value (Map.insert x newChild children))
    Just t -> insertWithHandler h def xs t fk $ \newChild ->
                 sk (Trie value (Map.insert x newChild children))

insert'' :: Ord c => [c] -> Trie c Bool -> a -> (Trie c Bool -> a) -> a
insert'' [] (Trie False cs) _ sk = sk (Trie True cs)
insert'' [] (Trie True _) fk _ = fk
insert'' (x:xs) Trie { .. } fk sk = 
  let mv = Map.lookup x children in
  case mv of
    Nothing -> insert'' xs (singleton False) fk $ \newChild ->
                 sk (Trie value (Map.insert x newChild children))
    Just t -> insert'' xs t fk $ \newChild ->
                 sk (Trie value (Map.insert x newChild children))


insert''' :: Ord c => [c] -> Int -> Trie c (Maybe Int) -> a -> (Trie c (Maybe Int) -> a) -> a
insert''' [] n (Trie Nothing cs) _ sk = sk (Trie (Just n) cs)
insert''' [] n t@(Trie (Just n') _) fk sk = 
  if n /= n' 
    then fk 
    else sk t
insert''' (x:xs) n Trie { .. } fk sk = 
  let mv = Map.lookup x children in
  case mv of
    Nothing -> insert''' xs n (singleton Nothing) fk $ \newChild ->
                 sk (Trie value (Map.insert x newChild children))
    Just t -> insert''' xs n t fk $ \newChild ->
                 sk (Trie value (Map.insert x newChild children))