module Tree23 
       (Tree23 (..),
        insert,
        find)
       where

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
data Carry a = Tr    (Tree23 a)
             | Rec a (Tree23 a) (Tree23 a)
               deriving (Show)

empty  = Empty
empty2 = (Empty, Empty)
empty3 = (Empty, Empty, Empty)

insert' :: (Ord a) => a -> Carry a -> Carry a
insert' x (Tr Empty) = Tr (TwoNode x empty2)

insert' x (Tr (TwoNode y (Empty,Empty)))
  | x == y    = Tr (TwoNode x empty2)
  | x < y     = Tr (ThreeNode (x,y) empty3)
  | otherwise = Tr (ThreeNode (y,x) empty3)

insert' x (Tr (TwoNode y (ly, gy)))
  | x == y             = Tr (TwoNode x (ly, gy))
  | x < y              =
    case (insert' x (Tr ly)) of
      (Tr tree)   -> Tr (TwoNode y (tree, gy))
      (Rec a l r) -> Tr (ThreeNode (a,y) (l,r,gy))
  | otherwise =
    case (insert' x (Tr gy)) of
      (Tr tree)   -> Tr (TwoNode y (ly, tree))
      (Rec a l r) -> Tr (ThreeNode (y,a) (ly, l, r))

insert' x (Tr (ThreeNode (y,z) (Empty, Empty, Empty)))
  | x == y      = Tr (ThreeNode (x,z) empty3)
  | x == z      = Tr (ThreeNode (y,x) empty3)
  | x<y         = Rec y (TwoNode x empty2) (TwoNode z empty2)
  | x>z         = Rec z (TwoNode y empty2) (TwoNode x empty2)                  
  | otherwise   = Rec x (TwoNode y empty2) (TwoNode z empty2)

insert' x (Tr (ThreeNode (y,z) (ly, byz, gz)))
  | x == y      = Tr (ThreeNode (x,z) (ly, byz, gz))
  | x == z      = Tr (ThreeNode (y,x) (ly, byz, gz))
  | x < y       =
    case (insert' x (Tr ly)) of
      (Tr tree)   -> Tr (ThreeNode (y,z) (tree, byz, gz))
      (Rec a l r) -> Rec y (TwoNode a (l,r)) (TwoNode z (byz, gz))
  | x > z       =
    case (insert' x (Tr gz)) of
      (Tr tree)   -> Tr (ThreeNode (y,z) (ly, byz, tree))
      (Rec a l r) -> Rec z (TwoNode y (ly, byz)) (TwoNode a (l,r))
  | otherwise   = 
    case (insert' x (Tr byz)) of
      (Tr tree)   -> Tr (ThreeNode (y,z) (ly, tree, gz))
      (Rec a l r) -> Rec a (TwoNode y (ly, l)) (TwoNode z (r,gz))

insert :: (Ord a) => a -> Tree23 a -> Tree23 a
insert x tree = balance $ insert' x (Tr tree)
  where
    balance (Tr tree) = tree
    balance (Rec a l r) = TwoNode a (l,r)

find :: (Ord a) => a -> Tree23 a -> Maybe a
find _ Empty = Nothing
find a (TwoNode x (l,g)) 
  | a == x = Just x
  | a < x  = find a l
  | a > x  = find a g
find a (ThreeNode (x,y) (lx, bxy, gy))
  | a == x = Just x
  | a == y = Just y
  | a < x  = find a lx
  | a > y  = find a gy
  | otherwise = find a bxy


fromList :: (Ord a) => [a] -> Tree23 a
fromList = foldr insert Empty

--TODO: not efficient, fixme
toList :: (Ord a) => Tree23 a -> [a]
toList Empty = []
toList (TwoNode x (l,g)) = toList l ++ [x] ++ toList g
toList (ThreeNode (x,y) (lx,bxy,gy)) = toList lx ++ [x] ++ toList bxy ++ [y] ++ toList gy
