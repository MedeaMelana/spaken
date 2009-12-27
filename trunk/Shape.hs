{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Shape (
  Shape(..), PointType(..), SomeShape(..),

  ShapeHist(..),

  toShape, toShapeHist,
  
  posOf, radius, center,

  someShape, typedPoint, typedLine, typedCircle, typedTwoPoints
) where

import MonadSpaken
import Vector

import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.Identity
-- import Data.Type.Equality  -- from package type-equality


-- Data type for shapes with calculated coördinates.

data Shape i where
  Point     :: PointType   -> Pos         -> Shape Point
  Line      :: Pos         -> Pos         -> Shape Line
  Circle    :: Pos         -> Double      -> Shape Circle
  TwoPoints :: Shape Point -> Shape Point -> Shape Points

{-
instance EqT Shape where
  eqT (Point t1 p1)   (Point t2 p2)  | t1 == t2 && p1 == p2
    = Just Refl
  eqT (Line  p1a p1b) (Line p2a p2b) | p1a == p2a && p1b == p2b
    = Just Refl
  eqT (Circle p1 r1)  (Circle p2 r2) | p1 == p2 && r1 == r2
    = Just Refl
  eqT (TwoPoints s1a s1b) (TwoPoints s2a s2b)
    = case (eqT s1a s2a, eqT s1b s2b) of
      (Just Refl, Just Refl) -> Just Refl
      _                      -> Nothing
  eqT _                   _
    = Nothing
-}

data PointType = Derived | Postulated
  deriving (Eq, Ord, Show, Read)

posOf :: Shape Point -> Pos
posOf (Point _ p) = p

radius :: Shape Circle -> Double
radius (Circle _ r) = r

center :: Shape Circle -> Pos
center (Circle p _) = p


-- A shape, together with its dependencies (transitive). This will be useful when drawing MonadSpaken expressions.

data ShapeHist i = ShapeHist
  { shape   :: Shape i       -- The Shape itself.
  , history :: Set SomeShape -- All Shapes that where used for calculating this Shape.
  } deriving Show

newtype ToShapeHist a = ToShapeHist (Identity a)
  deriving (Monad)

toShapeHist :: ToShapeHist a -> a
toShapeHist (ToShapeHist m) = runIdentity m

-- Lift a Shape to a ShapeHist with empty history.
noHist :: Shape i -> ShapeHist i
noHist = flip ShapeHist Set.empty

-- Merge two ShapeHists with a new Shape.
mergeHist :: Shape i -> ShapeHist a -> ShapeHist b -> ShapeHist i
mergeHist sn (ShapeHist s1 h1) (ShapeHist s2 h2) = ShapeHist sn hn
  where hn = Set.insert (someShape s1) $ Set.insert (someShape s2) $ Set.union h1 h2


instance MonadSpaken ToShapeHist ShapeHist where
  point p = return $ ShapeHist (Point Postulated p) Set.empty

  line p1 p2 = return $ mergeHist (Line (posOf $ shape p1) (posOf $ shape p2)) p1 p2

  circle p1 p2 = return $ mergeHist (Circle p1' (distance p1' p2')) p1 p2
    where p1'  = posOf $ shape p1
          p2'  = posOf $ shape p2

  intersectCC c1 c2 = return $ mergeHist (TwoPoints (Point Derived $ i (-1)) (Point Derived $ i 1)) c1 c2
    where p1   = center $ shape c1
          p2   = center $ shape c2
          r1   = radius $ shape c1
          r1s  = r1 * r1
          r2   = radius $ shape c2
          r2s  = r2 * r2
          dist = p2 - p1
          d    = magnitude dist
          xt   = (d * d - r2s + r1s) / (2 * d)
          yt   = sqrt ((-d + r2 - r1) * (-d - r2 + r1) * (-d + r2 + r1) * (d + r2 + r1)) / (2 * d)
          lens = p1 + xt *^ (normalized dist)
          i m  = lens + (m * yt) *^ (normalized $ cross2 dist)

  bothPoints (ShapeHist s h) = case s of
    (TwoPoints p1 p2) -> return (ShapeHist p1 h, ShapeHist p2 h)


-- Turn a MonadSpaken expression into a Shape, without the history stuff.

toShape :: ToShape i -> i
toShape (ToShape m) = runIdentity m

newtype ToShape a = ToShape (Identity a)
  deriving (Monad)

instance MonadSpaken ToShape Shape where
  point       = return . shape . toShapeHist . point
  line        = stripHist line
  circle      = stripHist circle
  intersectCC = stripHist intersectCC
  bothPoints (TwoPoints p1 p2) = return (p1, p2)

stripHist f p1 p2 = return . shape . toShapeHist $ f (noHist p1) (noHist p2)


-- Shape data type stripped of type information.

data SomeShape
  = APoint PointType Pos
  | ALine  Pos Pos
  | ACircle Pos Double
  | ATwoPoints SomeShape SomeShape
  deriving (Eq, Ord, Show, Read)

someShape :: Shape i -> SomeShape
someShape (Point t p)       = APoint t p
someShape (Line p1 p2)      = ALine p1 p2
someShape (Circle p r)      = ACircle p r
someShape (TwoPoints p1 p2) = ATwoPoints (someShape p1) (someShape p2)

typedPoint (APoint t p) = Point t p
typedLine (ALine p1 p2) = Line p1 p2
typedCircle (ACircle p r) = Circle p r
typedTwoPoints (ATwoPoints p1 p2) = TwoPoints (typedPoint p1) (typedPoint p2)

instance Show (Shape i) where
  show = filter (/= 'A') . show . someShape  -- evil hack!

