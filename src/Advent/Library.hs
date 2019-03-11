{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Advent.Library (
  defaultMain,

  pLines,
  brackets,
  parens,
  natural,
  integer,

  md5hash,

  push,
  pop,

  evolve,
  fromBuilders,
  applyAll,
  snoc, 
  for,
  fixedPoint,
  (!!?),

  Parser,
  module Text.Megaparsec,
  module Text.Megaparsec.Char
  ) where

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Data.Char (digitToInt)

import Data.Digest.Pure.MD5 (md5)
import qualified Data.ByteString.Lazy.Char8 as BS

import Data.Sequence as Seq
import Control.Monad.State

import Data.Void

type Parser = Parsec Void String

brackets :: Parser a -> Parser a
brackets = between (char '[') (char ']')

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

natural :: (Num a) => Parser a
natural = foldl (\acc n -> acc * 10 + fromIntegral (digitToInt n)) 0 <$> some digitChar

integer :: (Read a, Num a) => Parser a
integer = (char '-' *> (negate <$> integer))
      <|> char '+' *> integer
      <|> natural

pLines :: Parser a -> Parser [a]
pLines p = many (p <* eol)

md5hash :: String -> String
md5hash = show . md5 . BS.pack

pop :: (MonadState (Seq a) m) => m (Maybe a)
pop =
  Seq.viewl <$> get >>= \case
    EmptyL -> return Nothing
    a :< q' -> do
      put q'
      return $ Just a

push :: (MonadState (Seq a) m) => a -> m ()
push a = modify (|> a)

defaultMain :: String -> Parser a -> (a -> IO ()) -> IO ()
defaultMain description parser body = do
  input <- getContents
  case runParser (parser <* eof) description input of
    Left e -> putStrLn $ parseErrorPretty e
    Right a -> body a

evolve :: [a -> a] -> a -> [a]
evolve [] a = [a]
evolve (f:fs) a = a : evolve fs (f a)

fromBuilders :: a -> [a -> a] -> a
fromBuilders = foldl (flip ($))

applyAll = fromBuilders

snoc :: [a] -> a -> [a]
snoc ys x = foldr (:) [x] ys

for :: [a] -> (a -> b) -> [b]
for = flip map

fixedPoint :: (a -> a -> Bool) -> [a] -> a
fixedPoint eq = go
  where
    go (x:xs@(y:_)) | eq x y = x
                    | otherwise = go xs

infixl 9 !!?

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
(x:_) !!? 0 = Just x 
(_:xs) !!? n = xs !!? (n - 1)