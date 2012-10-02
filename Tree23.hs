module Tree23
       (Tree23 (..)
       , insert
       , lookup
--     , delete
       , fromList
       , toList)
       where

import Prelude hiding (lookup)


import Test.QuickCheck
import Control.Monad
import qualified Data.Set as S


data Tree23 a = Empty
              | N2 a ((Tree23 a), (Tree23 a))
              | N3 (a,a) ((Tree23 a), (Tree23 a), (Tree23 a))
                deriving (Eq)

instance (Show a) => Show (Tree23 a) where
  show = showPrefix "" ""
    where
      showPrefix _     _     Empty = ""
      showPrefix _     _     (N2 a (Empty,Empty)) =
        show a
      showPrefix pref1 pref2 (N2 a (l,r)) =
        show a ++ "\n"   ++
        pref1  ++ pref2  ++ "|--"  ++
          (showPrefix (pref1 ++ pref2) "|  "  l) ++ "\n" ++
        pref1  ++ pref2  ++ "`--"  ++ (showPrefix (pref1 ++ pref2) "   "  r)
      showPrefix pref1 pref2  (N3 (a,b) (Empty,Empty,Empty)) =
        show a ++ "\n"   ++
        pref1  ++ pref2  ++ show b
      showPrefix pref1 pref2 (N3 (a,b) (la,bab,gb)) =
        show a ++ "\n"   ++
        pref1  ++ pref2  ++ show b ++ "\n"   ++
        pref1  ++ pref2  ++ "|--"  ++
          (showPrefix (pref1 ++ pref2) "|  "  la)  ++ "\n" ++
        pref1  ++ pref2  ++ "|--"  ++
          (showPrefix (pref1 ++ pref2) "|  "  bab) ++ "\n" ++
        pref1  ++ pref2  ++ "`--"  ++
          (showPrefix (pref1 ++ pref2) "   "  gb)


data Builder a = Tr    (Tree23 a)
               | Rec a (Tree23 a) (Tree23 a)
                deriving (Show)

empty  = Empty
empty2 = (Empty, Empty)
empty3 = (Empty, Empty, Empty)

balanceL,balanceR,balanceM :: (Ord a) => Tree23 a -> Builder a -> Builder a

balanceL Empty _ = Tr Empty
balanceL _ (Tr Empty) = Tr Empty
balanceL (N2 y (ly, gy)) (Tr tree)    =
  Tr (N2 y (tree, gy))
balanceL (N2 y (ly, gy)) (Rec a l r)  =
  Tr (N3 (a,y) (l,r,gy))
balanceL (N3 (y,z) (ly, byz, gz)) (Tr tree) =
  Tr (N3 (y,z) (tree, byz, gz))
balanceL (N3 (y,z) (ly, byz, gz)) (Rec a l r) =
  Rec y (N2 a (l,r)) (N2 z (byz, gz))

balanceR _ t@(Tr Empty) = t
balanceR (N2 y (ly, gy)) (Tr tree)   = Tr (N2 y (ly, tree))
balanceR (N2 y (ly, gy)) (Rec a l r) = Tr (N3 (y,a) (ly, l, r))
balanceR (N3 (y,z) (ly, byz, gz)) (Tr tree) =
  Tr (N3 (y,z) (ly, byz, tree))
balanceR (N3 (y,z) (ly, byz, gz)) (Rec a l r) =
  Rec z (N2 y (ly, byz)) (N2 a (l,r))

balanceM Empty (Tr Empty) = Tr Empty
balanceM (N2 {}) _ = undefined
balanceM (N3 (y,z) (ly, byz, gz)) (Tr tree) =
  Tr (N3 (y,z) (ly, tree, gz))
balanceM (N3 (y,z) (ly, byz, gz)) (Rec a l r) =
  Rec a (N2 y (ly, l)) (N2 z (r,gz))


insert' :: (Ord a) => a -> Builder a -> Builder a
insert' x (Tr Empty) = Tr (N2 x empty2)

insert' x (Tr (N2 y (Empty,Empty)))
  | x == y    = Tr (N2 x empty2)
  | x < y     = Tr (N3 (x,y) empty3)
  | otherwise = Tr (N3 (y,x) empty3)

insert' x (Tr t@(N2 y (ly, gy)))
  | x == y    = Tr (N2 x (ly, gy))
  | x < y     = balanceL t (insert' x (Tr ly))
  | otherwise = balanceR t (insert' x (Tr gy))

insert' x (Tr (N3 (y,z) (Empty, Empty, Empty)))
  | x == y    = Tr (N3 (x,z) empty3)
  | x == z    = Tr (N3 (y,x) empty3)
  | x<y       = Rec y (N2 x empty2) (N2 z empty2)
  | x>z       = Rec z (N2 y empty2) (N2 x empty2)
  | otherwise = Rec x (N2 y empty2) (N2 z empty2)

insert' x (Tr t@(N3 (y,z) (ly, byz, gz)))
  | x == y    = Tr (N3 (x,z) (ly, byz, gz))
  | x == z    = Tr (N3 (y,x) (ly, byz, gz))
  | x < y     = balanceL t (insert' x (Tr ly))
  | x > z     = balanceR t (insert' x (Tr gz))
  | otherwise = balanceM t (insert' x (Tr byz))

insert :: (Ord a) => a -> Tree23 a -> Tree23 a
insert x tree = balance $ insert' x (Tr tree)

balance (Tr tree) = tree
balance (Rec a l r) = N2 a (l,r)

--delete' :: (Ord a) => a -> Builder a -> Builder a
--delete' x (Tr Empty) = Tr Empty
--delete' x (Tr (N2 y (ly, gy))) | y == x = Tr (N3
--                                    |
--delete' x (Tr (T

lookup :: (Ord a) => a -> Tree23 a -> Maybe a
lookup _ Empty = Nothing
lookup a (N2 x (l,g))
  | a == x = Just x
  | a < x  = lookup a l
  | a > x  = lookup a g
lookup a (N3 (x,y) (lx, bxy, gy))
  | a == x = Just x
  | a == y = Just y
  | a < x  = lookup a lx
  | a > y  = lookup a gy
  | otherwise = lookup a bxy
                
isElem x tree =
  case lookup x tree of
    Nothing -> False
    _       -> True             
    

fromList :: (Ord a) => [a] -> Tree23 a
fromList = foldr insert Empty

--TODO: not efficient, fixme
toList :: (Ord a) => Tree23 a -> [a]
toList Empty = []
toList (N2 x (l,g)) = toList l ++ [x] ++ toList g
toList (N3 (x,y) (lx,bxy,gy)) = toList lx  ++ [x] ++
                                       toList bxy ++ [y] ++
                                       toList gy

------------------------------------------------------------------
-- tests
------------------------------------------------------------------
instance (Ord a, Arbitrary a) => Arbitrary (Tree23 a) where
  arbitrary = sized $ \n ->
    do x   <- choose (0,n)
       lst <- sequence [ arbitrary | _ <- [1..x]]
       return$ fromList lst

prop_duplicates :: [Int] -> Bool
prop_duplicates x = (c == b)
   where
     a = foldr insert Empty x
     b = toList a
     c = uniq x
     uniq = S.toList . S.fromList

prop_duplicates2 :: Int -> Bool
prop_duplicates2 x = toList (fromList (replicate 1000 x)) == [x]

-- returns Just depth if tree is balanced
depth :: (Tree23 a) -> Maybe Int
depth Empty = Just 0
depth (N2 _ (l,g)) =
  case (depth l, depth g) of
    (Just x, Just y) ->
      if x == y then Just (1+x) else Nothing
    _ -> Nothing
depth (N3 _ (l,m,g)) =
  case (depth l, depth m, depth g) of
    (Just x, Just y, Just z) ->
      if x == y && y == z then Just (1+x) else Nothing
    _ -> Nothing

prop_balance :: Tree23 Int -> Bool
prop_balance x =
  case depth x of
    Nothing -> False
    Just _  -> True

prop_find :: Tree23 Int -> Int -> Bool
prop_find x y = case lookup y (insert y x) of
  Nothing -> False
  Just a  -> a == y

--prop_delete x y = prop_balance new_tree &&
--                  find y x /= Nothing
-- where new_tree = delete y x

main = do
  putStrLn "prop_find"        >> quickCheck prop_find
  putStrLn "prop_balance"     >> quickCheck prop_balance
  putStrLn "prop_duplicates2" >> quickCheck prop_duplicates2
  putStrLn "prop_duplicates"  >> quickCheck prop_duplicates
