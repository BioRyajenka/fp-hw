module Task3
       ( nextDay
       , afterDays
       , isWeekend
       , daysToParty

       , fight

       , Vector (..)
       , vLength
       , vSum
       , vDist
       , vScalP
       , vVectP

       , Tree (..)
       , tEmpty
       , tSize
       , tContains
       , tInsert
       , tCreate
       ) where

import           Data.List  (elemIndex, find)
import           Data.Maybe (fromJust)

{- Days Of Week -}

data DayOfWeek = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Eq, Enum, Show)

nextDay :: DayOfWeek -> DayOfWeek
nextDay d = case dropWhile (/= d) [Monday ..] of
    (_:next:_) -> next
    _          -> Monday

afterDays :: DayOfWeek -> Int -> DayOfWeek
afterDays startDay n = iterate nextDay startDay !! n

isWeekend :: DayOfWeek -> Bool
isWeekend d = d `elem` [Saturday, Sunday]

daysToParty :: DayOfWeek -> Int
daysToParty startDay = fromJust $ elemIndex Friday $ iterate nextDay startDay

{- Knights & Creeps -}

class Mob a where
    getAttack :: a -> Int
    getHP :: a -> Int
    reduceHP :: a -> Int -> a

fight :: (Mob a, Mob b) => a -> b -> (Either a b, Int)
fight fighter1 fighter2 = fight' where
      fight' = case lastStep of
          (_, winner, True)  -> (Right fighter2, getHP winner)
          (winner, _, False) -> (Left fighter1, getHP winner)
      steps = iterate doStep (fighter1, fighter2, True)
      lastStep = fromJust $ find (\(f1, f2, _) -> getHP f1 == 0 || getHP f2 == 0) steps

      doStep :: (Mob a, Mob b) => (a, b, Bool) -> (a, b, Bool)
      doStep (f1, f2, priority)
          | priority  = (f1, reduceHP f2 $ getAttack f1, False)
          | otherwise = (reduceHP f1 $ getAttack f2, f2, True)

data Knight = Knight {kAttack :: Int, kHP :: Int} deriving (Show)
instance Mob Knight where
    getAttack = kAttack
    getHP = kHP
    reduceHP k amount = k { kHP = max 0 (getHP k - amount) }

data Creep = Creep {cAttack :: Int, cHP :: Int} deriving (Show)
instance Mob Creep where
    getAttack = cAttack
    getHP = cHP
    reduceHP k amount = k { cHP = max 0 (getHP k - amount) }

{- Vectors -}

data Vector a = Vector2D a a | Vector3D a a a

vecToList :: Num a => Vector a -> [a]
vecToList (Vector2D x y)   = [x, y, 0]
vecToList (Vector3D x y z) = [x, y, z]

vLength :: Real a => Vector a -> Float
vLength v = sqrt $ sum $ map ((^^(2::Int)) . realToFrac) $ vecToList v

vSum :: Num a => Vector a -> Vector a -> Vector a
vSum a b = vSum' where
    vSum' = case (a, b) of
        (Vector2D _ _, Vector2D _ _) -> Vector2D (head res) (res!!1)
        _                            -> Vector3D (head res) (res!!1) (res!!2)
    res = zipWith (+) (vecToList a) (vecToList b)

vScalP :: Num a => Vector a -> Vector a -> a
vScalP a b = sum $ zipWith (*) (vecToList a) (vecToList b)

vInverse :: Num a => Vector a -> Vector a
vInverse (Vector2D x y)   = Vector2D (-x) (-y)
vInverse (Vector3D x y z) = Vector3D (-x) (-y) (-z)

vDist :: Real a => Vector a -> Vector a -> Float
vDist a b = vLength $ vSum a $ vInverse b

vVectP :: Num a => Vector a -> Vector a -> Maybe (Vector a)
vVectP (Vector3D ax ay az) (Vector3D bx by bz) = Just $ Vector3D (ay * bz - az * by) (az * bx - ax * bz) (ax * by - ay * bx)
vVectP _ _                                     = Nothing

{- Nat -}

data Nat = Z | S Nat

instance Eq Nat where
    (==) (S a) (S b) = a == b
    (==) Z Z         = True
    (==) _ _         = False

instance Ord Nat where
    (<=) (S a) (S b) = a <= b
    (<=) Z _         = True
    (<=) (S _) Z     = False

instance Num Nat where
    (+) Z y     = y
    (+) (S k) y = S (k + y)

    (*) Z _     = Z
    (*) (S k) y = y + k * y

    abs = id

    signum Z = 0
    signum _ = 1

    negate _ = raiseNegException

    fromInteger n
        | n < 0     = raiseNegException
        | n > 0     = S $ fromInteger $ n - 1
        | otherwise = Z

raiseNegException :: a
raiseNegException = error "Natural numbers can't be negative"

{-evenNat :: Nat -> Bool
evenNat Z     = True
evenNat (S x) = not $ evenNat x

divNat =

instance Integral Nat where
    quotRem n m = either (const (0, n)) (\x -> first S (quotRem x m)) (diff n m)

diff :: Nat -> Nat -> Either Nat Nat
diff (S n) (S m) = diff n m
diff n Z         = Right n
diff Z n         = Left  n
-}

{- Binary search tree -}

data Tree a = Leaf | Node a (Tree a) (Tree a)

tEmpty :: Tree a -> Bool
tEmpty Leaf = True
tEmpty _    = False

tSize :: Tree a -> Int
tSize Leaf         = 0
tSize (Node _ l r) = tSize l + tSize r

tContains :: (Ord a) => Tree a -> a -> Bool
tContains Leaf _ = False
tContains (Node v l r) x
    | x < v     = tContains l x
    | x > v     = tContains r x
    | otherwise = True

tInsert :: (Ord a) => Tree a -> a -> Tree a
tInsert Leaf x = Node x Leaf Leaf
tInsert (Node v l r) x
    | v < x     = Node v l (tInsert r x)
    | v > x     = Node v (tInsert l x) r
    | otherwise = Node v l r

tCreate :: Ord a => [a] -> Tree a
tCreate = foldl tInsert Leaf
