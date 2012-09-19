import Data.Function

data Trie a = End
          | Trie [(a, Trie a)]
          deriving (Show)

data Pair a b = Pair a b
               deriving (Show)

instance (Eq a) => Eq (Pair a b) where
  (Pair x _) == (Pair y _) = x == y

insert :: (Eq a) => [a] -> Trie a -> Trie a
insert [] trie  = trie
insert (x:xs) End = Trie$ [(x, (insert xs End))]
insert (x:xs) (Trie []) = Trie$ [(x,(insert xs End))]
insert (x:xs) (Trie ((y,tr):ys))
  | x == y = Trie$ (y,(insert xs tr)):ys
  | otherwise = Trie$ (x, (insert xs End)):(y,tr):ys

find :: (Eq a) => [a] -> Trie a -> Maybe [a]
find []   _          = Just []
find _    End        = Nothing
find _    (Trie [])  = Nothing
find (x:xs) t@(Trie ((y,tr):ys))
  | x == y =
    case find xs tr of
      Nothing -> Nothing
      Just z  -> Just (x:z)
  | otherwise = find (x:xs) (Trie ys)


a = insert "help" $ insert "hello" End