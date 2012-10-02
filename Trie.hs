{-# LANGUAGE DeriveGeneric #-}

import Data.Function
import GHC.Generics
import Data.Serialize
import Control.Applicative
import Data.Char
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import Data.List (foldl')
import Codec.Compression.GZip

data Trie a b = Tip  (Maybe b)
              | Trie (Maybe b) [(a, Trie a b)]
              deriving (Generic, Eq)
                       
instance (Serialize a, Serialize b) => Serialize (Trie a b)


insert :: (Eq a) => [a] -> b -> Trie a b -> Trie a b
insert [] v (Tip _)     = Tip  (Just v)
insert [] v (Trie _ l)  = Trie (Just v) l
insert (x:xs) v1 (Tip  v2)    = Trie v2 $ [(x, (insert xs v1 (Tip Nothing)))]
insert (x:xs) v1 (Trie v2 []) = Trie v2 $ [(x, (insert xs v1 (Tip Nothing)))]
insert (x:xs) v1 (Trie v2 ((y,tr):ys))
  | x == y =    Trie v2$ (y,(insert xs v1 tr)):ys
  | null ys   = Trie v2$ (x,(insert xs v1 (Tip Nothing))):(y,tr):[]
  | otherwise = (Trie v2 [(y,tr)]) `join` (insert (x:xs) v1 (Trie v2 ys))
   where
     join (Trie v1 [x]) (Trie _ ys) = Trie v1 (x:ys)
     join (Trie v1 xs) (Trie _ ys)  = Trie v1 (xs ++ ys)

find ::  (Eq a) => [a] -> Trie a b -> Maybe b
find []  (Tip  v)       = v
find xs  (Tip  v)       = Nothing
find []  (Trie v _)     = v
find xs  (Trie v [])    = Nothing
find (x:xs) (Trie v ((y,tr):ys))
  | x == y    = find xs tr 
  | otherwise = find (x:xs) (Trie v ys)


-- Examples

type WordCount = (Int, Trie Char Int)

emptyWordC = (1::Int, Tip Nothing)
insertWordC (n,trie) word = 
  case find word trie of
    Nothing -> (n+1, insert word n trie)
    Just x  -> (n,trie)
wordToInt (n,trie) word = find word trie

type WordFreq = Trie Char Int

emptyWordF = Tip Nothing :: WordFreq
insertWordF trie word = insertWordFN trie word 1
insertWordFN trie word n =
  case find word trie of
    Just k -> insert word (n+k) trie
    Nothing -> insert word n trie
  

enc x  = compress $ BL.fromChunks [encode x]
dec x  = decode $ BS.concat $ BL.toChunks (decompress x)

main2 = do 
  wrds <- (words . map toLower) <$> readFile "/usr/share/dict/words"
  let dict = foldl' insertWordC emptyWordC wrds
  BL.writeFile "./words.dict.gz" (enc dict)

readDict :: IO (Either String WordCount)
readDict = do
  bytes <- BL.readFile "./words.dict.gz"
  return (dec bytes)
  
main = do
  lns <- lines <$> readFile "./goog2.csv"
  let dict = foldl' insertWordF emptyWordF lns
  BL.writeFile "./goog.dict.gz" (enc dict)
  print "done"