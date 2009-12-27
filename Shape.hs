{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Shape (
  Shape(..), PointType(..),

  toShape,
  
  posOf, radius, center
) where

import MonadSpaken
import Vector

import Control.Monad.Identity

-- Data type for shapes with calculated coördinates.

data Shape i where
  Point     :: PointType   -> Pos         -> Shape Point
  Line      :: Pos         -> Pos         -> Shape Line
  Circle    :: Pos         -> Double      -> Shape Circle
  TwoPoints :: Shape Point -> Shape Point -> Shape Points

data PointType = Derived | Postulated
  deriving (Eq, Show, Read)

posOf :: Shape Point -> Pos
posOf (Point _ p) = p

radius :: Shape Circle -> Double
radius (Circle _ r) = r

center :: Shape Circle -> Pos
center (Circle p _) = p

-- Use toShape on a generic MonadSpaken-thingy to get the result as a Shape.

toShape :: ToShape i -> i
toShape (ToShape m) = runIdentity m

newtype ToShape a = ToShape (Identity a)
  deriving (Monad)

instance MonadSpaken ToShape Shape where
  point        = return . Point Postulated
  line   p1 p2 = return $ Line (posOf p1) (posOf p2)
  circle p1 p2 = return $ Circle p1' (distance p1' p2')
    where p1'  = posOf p1
          p2'  = posOf p2
  intersectCC c1 c2 = return $ TwoPoints (Point Derived $ i (-1)) (Point Derived $ i 1)
    where i m  = (m * yt) *^ (lens + (normalized $ cross2 dist))
          lens = p1 + xt *^ (normalized dist)
          dist = p2 - p1
          p1   = center c1
          p2   = center c2
          r1   = radius c1
          r1s  = r1 * r1
          r2   = radius c2
          r2s  = r2 * r2
          d    = magnitude dist
          xt   = (d * d - r2s + r1s) / (2 * d)
          yt   = sqrt ((-d + r2 - r1) * (-d -r2 + r1) * (-d + r2 + r1) * (d + r2 + r1)) / (2 * d)
  bothPoints (TwoPoints p1 p2) = return (p1, p2)

-- Shape data type stripped of type information.
-- Primarily for 'easy' deriving of Show. Probably overkill.

data StrippedShape
  = APoint PointType Pos
  | ALine  Pos Pos
  | ACircle Pos Double
  | ATwoPoints StrippedShape StrippedShape
  deriving (Eq, Show, Read)

strip :: Shape i -> StrippedShape
strip (Point t p)       = APoint t p
strip (Line p1 p2)      = ALine p1 p2
strip (Circle p r)      = ACircle p r
strip (TwoPoints p1 p2) = ATwoPoints (strip p1) (strip p2)

instance Show (Shape i) where
  show = show . strip

