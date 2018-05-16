import Data.List

-- Write a function that computes the number of elements in a list.
-- To test it, ensure that it gives the same answers as the standard length
-- function.
length' :: [a] -> Int
length' []     = 0
length' (_:xs) = 1 + length' xs

-- Write a function that computes the mean of a list, i.e. the sum of all
-- elements in the list divided by its length. (You may need to use the
-- fromIntegral function to convert the length of the list from an integer into
-- a floating point number.)
-- mean :: [a] -> Fractional -- FIXME type signature nya gimana?
sum' :: (Num a) => [a] -> a
sum' (x:xs) = x + sum' xs
sum' []     = 0

mean :: Fractional a => [a] -> a
mean [] = 0
mean xs = sum' xs / fromIntegral(length xs)

-- Turn a list into a palindrome, i.e. it should read the same both backwards
-- and forwards. For example, given the list [1,2,3], your function should
-- return [1,2,3,3,2,1].
palindrome xs = xs ++ reverse xs

palindrome' :: [a] -> [a]
palindrome' []     = []
palindrome' (x:xs) = x : palindrome' xs ++ [x]

-- Write a function that determines whether its input list is a palindrome
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- Create a function that sorts a list of lists based on the length of each
-- sublist. (You may want to look at the sortBy function from the Data.List
-- module.)
sortSubList :: [[a]] -> [[a]]
sortSubList xs = sortBy compareLength xs
    where compareLength a b = compare (length a) (length b)

-- Define a function that joins a list of lists together using a separator
-- value.
intersperse :: a -> [[a]] -> [a]
intersperse _ [] = []
intersperse _ (x:[]) = x
intersperse sep (x:xs) = x ++ [sep] ++ Main.intersperse sep xs

-- Using the binary tree type that we defined earlier in this chapter, write a
-- function that will determine the height of the tree. The height is the
-- largest number of hops from the root to an Empty . For example, the tree
-- Empty has height zero; Node "x" Empty Empty has height one; Node "x" Empty
-- (Node "y" Empty Empty) has height two; and so on.
data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Node _ Empty Empty) = 1
treeHeight (Node _ a b) = 1 + (max (treeHeight a) (treeHeight b))

-- Consider three two-dimensional points, a, b, and c. If we look at the angle
-- formed by the line segment from a to b and the line segment from b to c, it
-- turns left, turns right, or forms a straight line. Define a Direction data
-- type that lets you represent these possibilities.
data Direction = LEFT | RIGHT | STRAIGHT
                 deriving (Show, Eq)
data Point = Point {
              x :: Int,
              y :: Int
             }
             deriving (Show, Eq)

directionOf :: Point -> Point -> Point -> Direction
directionOf (Point ax ay) (Point bx by) (Point cx cy) =
            case sign of
              GT -> LEFT
              LT -> RIGHT
              _  -> STRAIGHT
            where calc = (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)
                  sign = compare calc 0

-- Define a function that takes a list of two-dimensional points and computes
-- the direction of each successive triple. Given a list of points [a,b,c,d,e],
-- it should begin by computing the turn made by [a,b,c], then the turn made by
-- [b,c,d], then [c,d,e]. Your function should return a list of Direction.
directionsOf :: [Point] -> [Direction]
directionsOf (a:b:c:[]) = directionOf a b c : []
directionsOf (a:b:c:xs) = directionOf a b c : directionsOf (b:c:xs)

-- Using the code from the preceding three exercises, implement Graham’s scan
-- algorithm for the convex hull of a set of 2D points. You can find good
-- description of what a convex hull (http://en.wikipedia.org/wiki/Convex_hull)
-- is, and how the Graham scan algorithm
-- (http://en.wikipedia.org/wiki/Graham_scan) should work, on Wikipedia
-- (http://en.wikipedia.org/).
