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
              | TwoNode a ((Tree23 a), (Tree23 a))
              | ThreeNode (a,a) ((Tree23 a), (Tree23 a), (Tree23 a))
                deriving (Eq)

instance (Show a) => Show (Tree23 a) where
  show = showTree 0

tabs n = replicate n '\t'
showTree n (Empty) = ""
showTree n (TwoNode a (Empty,Empty)) = tabs n ++ show a ++ "::"
showTree n (TwoNode a (l,r)) = tabs n ++ show a
                                      ++ ":\n" ++ tabs n ++ showTree (n+1) l
                                      ++ "\n" ++ tabs n ++ showTree (n+1) r
showTree n (ThreeNode (a,b) (Empty, Empty, Empty)) = tabs n ++ show (a,b) ++ "::"
showTree n (ThreeNode (a,b) (la, bab, gb)) = tabs n ++ show (a,b)
                                                    ++ ":\n" ++ tabs n ++ showTree (n+1) la
                                                    ++ "\n" ++ tabs n ++ showTree (n+1) bab
                                                    ++ "\n" ++ tabs n ++ showTree (n+1) gb

-- NOTE: can this be turned into a Monad?
data Builder a = Tr    (Tree23 a)
               | Rec a (Tree23 a) (Tree23 a)
                deriving (Show)

empty  = Empty
empty2 = (Empty, Empty)
empty3 = (Empty, Empty, Empty)

balanceL,balanceR,balanceM :: (Ord a) => Tree23 a -> Builder a -> Builder a
balanceL Empty _ = Tr Empty
balanceL _ (Tr Empty) = Tr Empty
balanceL t@(TwoNode y (ly, gy)) (Tr tree)    =
  Tr (TwoNode y (tree, gy))
balanceL t@(TwoNode y (ly, gy)) (Rec a l r)  =
  Tr (ThreeNode (a,y) (l,r,gy))
balanceL t@(ThreeNode (y,z) (ly, byz, gz)) (Tr tree) =
  Tr (ThreeNode (y,z) (tree, byz, gz))
balanceL t@(ThreeNode (y,z) (ly, byz, gz)) (Rec a l r) =
  Rec y (TwoNode a (l,r)) (TwoNode z (byz, gz))


balanceR _ t@(Tr Empty) = t
balanceR t@(TwoNode y (ly, gy)) (Tr tree)   = Tr (TwoNode y (ly, tree))
balanceR t@(TwoNode y (ly, gy)) (Rec a l r) = Tr (ThreeNode (y,a) (ly, l, r))
balanceR t@(ThreeNode (y,z) (ly, byz, gz)) (Tr tree) =
  Tr (ThreeNode (y,z) (ly, byz, tree))
balanceR t@(ThreeNode (y,z) (ly, byz, gz)) (Rec a l r) =
  Rec z (TwoNode y (ly, byz)) (TwoNode a (l,r))


balanceM Empty (Tr Empty) = Tr Empty
balanceM t@(TwoNode {}) _ = undefined
balanceM t@(ThreeNode (y,z) (ly, byz, gz)) (Tr tree) =
  Tr (ThreeNode (y,z) (ly, tree, gz))
balanceM t@(ThreeNode (y,z) (ly, byz, gz)) (Rec a l r) =
  Rec a (TwoNode y (ly, l)) (TwoNode z (r,gz))


insert' :: (Ord a) => a -> Builder a -> Builder a
insert' x (Tr Empty) = Tr (TwoNode x empty2)

insert' x (Tr (TwoNode y (Empty,Empty)))
  | x == y    = Tr (TwoNode x empty2)
  | x < y     = Tr (ThreeNode (x,y) empty3)
  | otherwise = Tr (ThreeNode (y,x) empty3)

insert' x (Tr t@(TwoNode y (ly, gy)))
  | x == y             = Tr (TwoNode x (ly, gy))
  | x < y              = balanceL t (insert' x (Tr ly))
  | otherwise          = balanceR t (insert' x (Tr gy))

insert' x (Tr (ThreeNode (y,z) (Empty, Empty, Empty)))
  | x == y      = Tr (ThreeNode (x,z) empty3)
  | x == z      = Tr (ThreeNode (y,x) empty3)
  | x<y         = Rec y (TwoNode x empty2) (TwoNode z empty2)
  | x>z         = Rec z (TwoNode y empty2) (TwoNode x empty2)
  | otherwise   = Rec x (TwoNode y empty2) (TwoNode z empty2)

insert' x (Tr t@(ThreeNode (y,z) (ly, byz, gz)))
  | x == y      = Tr (ThreeNode (x,z) (ly, byz, gz))
  | x == z      = Tr (ThreeNode (y,x) (ly, byz, gz))
  | x < y       = balanceL t (insert' x (Tr ly))
  | x > z       = balanceR t (insert' x (Tr gz))
  | otherwise   = balanceM t (insert' x (Tr byz))

insert :: (Ord a) => a -> Tree23 a -> Tree23 a
insert x tree = balance $ insert' x (Tr tree)

balance (Tr tree) = tree
balance (Rec a l r) = TwoNode a (l,r)

--delete' :: (Ord a) => a -> Builder a -> Builder a
--delete' x (Tr Empty) = Tr Empty
--delete' x (Tr (TwoNode y (ly, gy))) | y == x = Tr (ThreeNode
--                                    | 
--delete' x (Tr (T

isElem x tree =
  case lookup x tree of
    Nothing -> False
    _       -> True


lookup :: (Ord a) => a -> Tree23 a -> Maybe a
lookup _ Empty = Nothing
lookup a (TwoNode x (l,g))
  | a == x = Just x
  | a < x  = lookup a l
  | a > x  = lookup a g
lookup a (ThreeNode (x,y) (lx, bxy, gy))
  | a == x = Just x
  | a == y = Just y
  | a < x  = lookup a lx
  | a > y  = lookup a gy
  | otherwise = lookup a bxy


fromList :: (Ord a) => [a] -> Tree23 a
fromList = foldr insert Empty

--TODO: not efficient, fixme
toList :: (Ord a) => Tree23 a -> [a]
toList Empty = []
toList (TwoNode x (l,g)) = toList l ++ [x] ++ toList g
toList (ThreeNode (x,y) (lx,bxy,gy)) = toList lx  ++ [x] ++
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
depth (TwoNode _ (l,g)) =
  case (depth l, depth g) of
    (Just x, Just y) ->
      if x == y then Just (1+x) else Nothing
    _ -> Nothing
depth (ThreeNode _ (l,m,g)) =
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
