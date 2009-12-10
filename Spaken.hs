{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Spaken where

import Prelude hiding (id, (.), fst, snd)
import Control.Category
import Control.Arrow


-- Primitive elements

data Point
data Line
data Circle
data Points


-- Constructions

data Spaken a b where
  -- Category combinators
  Id     :: Spaken a a
  Comp   :: Spaken b c -> Spaken a b -> Spaken a c

  -- Arrow combinators
  First  :: Spaken a b -> Spaken (a, c) (b, c)
  Second :: Spaken a b -> Spaken (c, a) (c, b)
  Fanout :: Spaken a b -> Spaken a c -> Spaken a (b, c)

  -- Projections
  Fst    :: Spaken (a, b) a
  Snd    :: Spaken (a, b) b

  -- Elementary constructors
  ArbitraryPoint :: PointPred a -> Spaken a Point
  Line           :: Spaken (Point, Point) Line
  Circle         :: Spaken (Point, Point) Circle

instance Category Spaken where
  id  = Id
  (.) = Comp

instance Arrow Spaken where
  arr     = error "Spaken does not support arbitrary functions"
  first   = First
  second  = Second
  f *** g = first f >>> second g
  (&&&)   = Fanout


data PointPred a where
  Anywhere :: PointPred ()
  OnLine   :: PointPred Line
  OnCircle :: PointPred Circle
  InSet    :: PointPred Points
  SameSide :: PointPred (Line :& Point)  -- ^ Lies on same side of line as given point.
  -- EqPoint  :: PointPred Point
  -- Conj     :: PointPred a -> PointPred b -> PointPred (a :& b)
  -- Not      :: PointPred a -> PointPred a


-- Derived arrow combinators.

fst = Fst
snd = Snd

swap :: Spaken (a, b) (b, a)
swap = snd &&& fst


-- Elementary constructors.

arbitraryPoint :: PointPred a -> Spaken a Point
arbitraryPoint = ArbitraryPoint

line :: Spaken (Point, Point) Line
line = Line

circle :: Spaken (Point, Point) Circle
circle = Circle


-- Elementary destructors.

deCircle :: Spaken Circle (Point :& Point)
deCircle = undefined

deLine :: Spaken Line (Point :& Point)
deLine = undefined

-- Intersections.

intersectCC :: Spaken (Circle, Circle) (Point, Point)
intersectCC = undefined

intersectLL :: Spaken (Line :& Line) Point
intersectLL = undefined

intersectLC :: Spaken (Line :& Circle) (Point :& Point)
intersectLC = undefined


-- data a :& b = a :& b
type (:&) = (,)
infixr :&


-- Some theorems.

-- | Construct a circle from three points on its circumference.
circle3 :: Spaken (Point :& Point :& Point) Circle
circle3 = undefined

-- Here's a first attempt at circle3 in arrow notation:
-- circle3 = ((second fst >>> bisection) &&& (snd >>> bisection)) >>> intersectLL

-- Here is circle3 in monadic notation:
-- circle3 (p1 :& p2 :& p3) = do
--   l1 <- bisection (p1 :& p2)
--   l2 <- bisection (p2 :& p3)
--   pc <- intersectLL (l1 :& l2)
--   circle (pc, p1)

-- | The compass equivalence theorem says that a specific circle maybe copied to another location.
--   See: http://en.wikipedia.org/wiki/Compass_equivalence_theorem
--   Invariant: snd == compassEquivalence >>> circleCenter
compassEquivalence :: Spaken (Circle :& Point) Circle
compassEquivalence = undefined

-- | Construct a line through a point parallel to another line.
parallel :: Spaken (Line :& Point) Line
parallel = undefined

bisection :: Spaken (Point, Point) Line
bisection = (circle &&& (swap >>> circle)) >>> intersectCC >>> line

-- | http://mathworld.wolfram.com/MalfattisProblem.html
malfatti :: Spaken (Point :& Point :& Point) (Circle :& Circle :& Circle)
malfatti = undefined

-- | http://en.wikipedia.org/wiki/Constructible_polygon
pentagon :: Spaken (Point :& Point) (Line :& Line :& Line :& Line :& Line)
pentagon = undefined


-- Serialization of constructions.

data Ix ix where
  IxPoint  :: Ix Point
  IxLine   :: Ix Line
  IxCircle :: Ix Circle
  IxTuple  :: Ix a -> Ix b -> Ix (a, b)

data SomeIx where
  SomeIx :: Ix ix -> SomeIx

showSomeIx :: SomeIx -> String
showSomeIx (SomeIx ix) = case ix of
  IxPoint  -> "IxPoint"
  IxLine   -> "IxLine"
  IxCircle -> "IxCircle"

readSomeIx :: String -> Maybe SomeIx
readSomeIx input = case input of
  "IxPoint"  -> Just (SomeIx IxPoint)
  "IxLine"   -> Just (SomeIx IxLine)
  "IxCircle" -> Just (SomeIx IxCircle)


data (a :=: b) where
  Refl :: a :=: a

data SomeSpaken where
  SomeSpaken :: Ix a -> Ix b -> Spaken a b -> SomeSpaken
