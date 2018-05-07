module W3 where

-- Week 3:
--   * built-in datatypes
--   * custom datatypes
--   * folds
--
-- Useful functions / types:
--   * Maybe
--
-- Don't add any new imports!

------------------------------------------------------------------------------
-- Ex 1: implement safe integer devision, that is, a function that
-- returns Just result normally, but Nothing if the divisor is zero.
--
-- Remember that integer division can be done with the div function.

safeDiv :: Integer -> Integer -> Maybe Integer
safeDiv x 0 = Nothing
safeDiv x y = Just (div x y)

------------------------------------------------------------------------------
-- Ex 2: another variant of safe division. This time a succesful
-- division should be returned as
--   Right result
-- whereas a division by zero should return
--   Left "1234/0"
-- (replace 1234 with the value of x).

eitherDiv :: Integer -> Integer -> Either String Integer
eitherDiv x 0 = Left ((show x) ++ "/0")
eitherDiv x y = Right (div x y)

------------------------------------------------------------------------------
-- Ex 3: implement the function addEithers, which combines two values of type
-- Either String Int into one like this:
--
-- - If both inputs were Ints, sum the Ints
-- - Otherwise, return the first argument that was not an Int
--
-- Exmaples:
--
-- addEithers (Right 1) (Right 2) ==> Right 3
-- addEithers (Right 1) (Left "fail") ==> Left "fail"
-- addEithers (Left "boom") (Left "fail") ==> Left "boom"

addEithers :: Either String Int -> Either String Int -> Either String Int
addEithers (Right a) (Right b) = Right (a + b)
addEithers (Left a) _ = Left a
addEithers _ (Left b) = Left b

------------------------------------------------------------------------------
-- Ex 4: implement the function mapMaybe, which works a bit like a
-- combined map & filter.
---
-- mapMaybe is given a list ([a]) and a function of type a -> Maybe b.
-- This function is called for all values in the list. If the function
-- returns Just x, x will be in the result list. If the function
-- returns Nothing, nothing gets added to the result list.
--
-- Examples:
--
-- let f x = if x>0 then Just (2*x) else Nothing
-- in mapMaybe f [0,1,-1,4,-2,2]
--   ==> [2,8,4]
--
-- mapMaybe Just [1,2,3]
--   ==> [1,2,3]
--
-- mapMaybe (\x -> Nothing) [1,2,3]
--   ==> []

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f xs = mapMaybe' f xs []

mapMaybe' :: (a -> Maybe b) -> [a] -> [b] -> [b]
mapMaybe' f (x:xs) m = case f x of Nothing -> mapMaybe' f xs m
                                   Just y -> mapMaybe' f xs (m ++ [y])
mapMaybe' f [] m = m
------------------------------------------------------------------------------
-- Ex 5: define a datatype Person, which should contain the age (an
-- Int) and the name (a String) of a person.
--
-- Also define a Person value fred, and the functions getAge, getname,
-- setAge and setName (see below).

data Person = Person {age::Int, name:: String}
  deriving Show

-- fred is a person whose name is Fred and age is 90
fred :: Person
fred = Person 90 "Fred"

-- getName returns the name of the person
getName :: Person -> String
getName (Person _ name) = name

-- getAge returns the age of the person
getAge :: Person -> Int
getAge (Person age _) = age

-- setName takes a person and returns a new person with the name changed
setName :: String -> Person -> Person
setName name p = Person (getAge p) name

-- setAge does likewise for age
setAge :: Int -> Person -> Person
setAge age p = Person age (getName p)

------------------------------------------------------------------------------
-- Ex 6&7: define a datatype TwoCounters which contains two Int
-- counters. Also define the functions below for operating a
-- TwoCounters.
--
-- This exercise is worth two points.
--
-- Examples:
--
-- getA (incA (incA zeros))
--   ==> 2
-- getB (incB (incA zeros))
--   ==> 1

data TwoCounters = TwoCounters Int Int

-- zeros is a TwoCounters value with both counters initialized to 0
zeros :: TwoCounters
zeros = TwoCounters 0 0

-- getA returns the value of the "A" counter
getA :: TwoCounters -> Int
getA (TwoCounters a _) = a

-- getB returns the value of the "B" counter
getB :: TwoCounters -> Int
getB (TwoCounters _ b) = b

-- incA increases the value of the "A" counter by one
incA :: TwoCounters -> TwoCounters
incA tc = TwoCounters ((getA tc)+1) (getB tc)

-- incB does likewise for the "B" counter
incB :: TwoCounters -> TwoCounters
incB tc = TwoCounters (getA tc) ((getB tc)+1)

------------------------------------------------------------------------------
-- Ex 8&9: define a datatype UpDown that represents a counter that can
-- either be in incresing or decreasing mode. Also implement the
-- functions zero, toggle, tick and get below.
--
-- NB! Define _two_ constructors for your datatype, in other words,
-- the definition should be of the form
--   data UpDown = A something | B foobar
--
-- This exercise is worth two points
--
-- Examples:
--
-- get (tick zero)
--   ==> 1
-- get (tick (tick zero))
--   ==> 2
-- get (tick (tick (toggle (tick zero))))
--   ==> -1

data UpDown = Up Int | Down Int

-- zero is an increasing counter with value 0
zero :: UpDown
zero = Up 0 

-- get returns the counter value
get :: UpDown -> Int
get (Up value)   = value
get (Down value) = value

-- tick increases an increasing counter by one or decreases a
-- decreasing counter by one
tick :: UpDown -> UpDown
tick (Up value)   = Up (value +1)
tick (Down value) = Down (value -1) 

-- toggle changes an increasing counter into a decreasing counter and
-- vice versa
toggle :: UpDown -> UpDown
toggle (Up value)   = Down value
toggle (Down value) = Up value

--------------------------------------------------------------------------------
-- The next exercises use the binary tree type defined like this:

data Tree a = Leaf | Node a (Tree a) (Tree a)
            deriving (Show, Eq)

-- Ex 10: implement the function valAtRoot which returns the value at
-- the root (top-most node) of the tree. The return value is Maybe a
-- because the tree might be empty (i.e. just a Leaf)

valAtRoot :: Tree a -> Maybe a
valAtRoot (Leaf)       = Nothing
valAtRoot (Node a _ _) = Just a

------------------------------------------------------------------------------
-- Ex 11: compute the size of a tree, that is, the number of Node
-- constructors in it

treeSize :: Tree a -> Int
treeSize (Leaf) = 0
treeSize (Node a b c) = (treeSize b) + (treeSize c) + 1

------------------------------------------------------------------------------
-- Ex 12: get the leftmost value in the tree. The return value is
-- Maybe a because the tree might be empty.
--
-- The leftmost value means the value that is reached by going to the
-- left child of the current Node for as long as possible.
--
-- Examples:
--
-- leftest Leaf
--   ==> Nothing
-- leftest (Node 1 (Node 2 (Node 3 Leaf Leaf) Leaf) Leaf)
--   ==> Just 3
-- leftest (Node 1 (Node 2 Leaf (Node 3 Leaf Leaf)) (Node 4 Leaf Leaf))
--   ==> Just 2

leftest :: Tree a -> Maybe a
leftest (Leaf) = Nothing
leftest (Node a (Leaf) _) = Just a
leftest (Node a b _) = leftest b

------------------------------------------------------------------------------
-- Ex 13: implement map for trees.
--
-- Examples:
--
-- mapTree (+1) Leaf  ==>  Leaf
-- mapTree (+2) (Node 0 (Node 1 Leaf Leaf) (Node 2 Leaf Leaf))
--   ==> (Node 2 (Node 3 Leaf Leaf) (Node 4 Leaf Leaf))

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Node a b c)  = (Node (f a) (mapTree f b) (mapTree f c))
mapTree f (Leaf) = Leaf

------------------------------------------------------------------------------
-- Ex 14: insert the given value into the leftmost possible place. You
-- need to return a new tree since the function is pure.
--
-- Example:
-- insertL 0 Leaf
--   ==> Node 0 Leaf Leaf
-- insertL 0 (Node 1 Leaf Leaf)
--   ==> Node 1 (Node 0 Leaf Leaf) Leaf)
--
-- insertL 0 (Node 1
--             (Node 2
--               Leaf
--               (Node 3 Leaf Leaf))
--             (Node 4 Leaf Leaf))
--
--        ==> Node 1
--             (Node 2
--               (Node 0 Leaf Leaf)
--               (Node 3 Leaf Leaf))
--             (Node 4 Leaf Leaf)


insertL :: a -> Tree a -> Tree a
insertL v Leaf = Node v Leaf Leaf
insertL v (Node x Leaf r) = Node x (Node v Leaf Leaf) r
insertL v (Node x l r) = Node x (insertL v l) r 

------------------------------------------------------------------------------
-- Ex 15: implement the function measure, that takes a tree and
-- returns a tree with the same shape, but with the value at every
-- node being the size of the subtree starting at that node.
--
-- Tree size is defined as in Ex 11
--
-- Examples:
--
-- measure (Node 'a' Leaf Leaf)
--  ==> Node 1 Leaf Leaf
-- measure (Node 'a' (Node 'b' Leaf Leaf) Leaf)
--  ==> Node 2 (Node 1 Leaf Leaf) Leaf
-- measure (Node 0 (Node 0 Leaf Leaf) Leaf)
--  ==> Node 2 (Node 1 Leaf Leaf) Leaf
-- measure (Node 0 (Node 0 Leaf Leaf)
--                 (Node 0 (Node 0 Leaf Leaf)
--                         (Node 0 Leaf
--                                 (Node 0 Leaf Leaf))))
--      ==> Node 6 (Node 1 Leaf Leaf)
--                 (Node 4 (Node 1 Leaf Leaf)
--                         (Node 2 Leaf
--                                 (Node 1 Leaf Leaf)))


measure :: Tree a -> Tree Int
measure (Leaf) = Leaf
measure (Node a left right) = Node depth (measure left) (measure right)
    where depth = 1 + (treeSize left) + (treeSize right)

------------------------------------------------------------------------------
-- Ex 16: the standard library function
--   foldr :: (a -> b -> b) -> b -> [a] -> b
-- is used to "collapse" a list to a single value, like this:
--   foldr f start [x,y,z,w]
--     ==> f x (f y (f z (f w start)
--
-- Implement the functions sumf and lengthf so that mysum computes the
-- sum of the values in the list and mylength computes the length of
-- the list.
--
-- DON'T change the definitions of mysum and mylength, only implement
-- sumf and lengtf appropriately.

mysum :: [Int] -> Int
mysum is = foldr sumf 0 is

sumf :: Int -> Int -> Int
sumf x y = x + y

mylength :: [a] -> Int
mylength xs = foldr lengthf 0 xs

lengthf :: a -> Int -> Int
lengthf x y = y + 1

------------------------------------------------------------------------------
-- Ex 17: implement the function foldTree that works like foldr, but
-- for Trees.
--
-- Example:
--   foldTree f l (Node 3 Leaf Leaf)
--     ==> f 3 l l
--   foldTree f l (Node 'a' (Node 'b' (Node 'c' Leaf Leaf)
--                                    Leaf)
--                          (Node 'd' Leaf Leaf))
--     ==> f (f 'a' (f 'b' (f 'c' l l)
--                         l)
--                  (f 'd' l l))
--
-- Once you've implemented foldTree correctly, the functions treeSum
-- and treeLeaves below work correctly.

sumt :: Int -> Int -> Int -> Int
sumt x y z = x+y+z

-- Sum of numbers in the tree
treeSum :: Tree Int -> Int
treeSum t = foldTree sumt 0 t

leaft :: a -> Int -> Int -> Int
leaft x y z = y+z

-- Number of leaves in the tree
treeLeaves :: Tree a -> Int
treeLeaves t = foldTree leaft 1 t

foldTree :: (a -> b -> b -> b) -> b -> Tree a -> b
foldTree _ b Leaf                = b
foldTree f b (Node a left right) = f a left' right'
  where  left'  = foldTree f b left
         right' = foldTree f b right

------------------------------------------------------------------------------
-- Ex 18: You'll find a Color datatype below. It has the three basic
-- colours Red, Green and Blue, and two color transformations, Mix and
-- Darken.
--
-- Mix means the sum of the two colors in each rgb channel.
--
-- Darken means darkening the color. The Double value tells how much
-- the color is darkened. 0.0 means no change and 1.0 means full
-- darkening, i.e. black. Darkening by 0.1 will have the result of
-- each channel decreasing by 10%.
--
-- Implement the function rgb :: Color -> [Double] that returns a list
-- of length three that represents the rgb value of the given color.
--
-- Examples:
--
-- rgb Red   ==> [1,0,0]
-- rgb Green ==> [0,1,0]
-- rgb Blue  ==> [0,0,1]
--
-- rgb (Mix Red Green)                ==> [1,1,0]
-- rgb (Mix Red (Mix Red Green))      ==> [1,1,0]
-- rgb (Darken 0.2 Red)               ==> [0.8,0,0]
-- rgb (Darken 0.2 (Darken 0.2 Red))  ==> [0.64,0,0]
-- rgb (Mix (Darken 0.4 Red) (Darken 0.4 Red)) ==> [1,0,0]
-- rgb (Mix (Darken 0.6 Red) (Darken 0.6 Red)) ==> [0.8,0,0]
--
-- NB! Mix should saturate at 1.0

data Color = Red | Green | Blue | Mix Color Color | Darken Double Color
  deriving Show

rgb :: Color -> [Double]
rgb Red          = [1, 0, 0]
rgb Green        = [0, 1, 0]
rgb Blue         = [0, 0, 1]
rgb (Darken constant a) = [if x <= 0 then 0 else x | x <-(map(\y -> y * (1-constant)) (rgb a))]
rgb (Mix a b)    = [if x > 1 then 1 else x  | x <-(zipWith (+) (rgb a) (rgb b))]