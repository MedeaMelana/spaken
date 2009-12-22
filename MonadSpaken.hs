{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MonadSpaken where

import Control.Monad.State.Strict
import Control.Arrow (first, second)
import Data.Type.Equality  -- from package type-equality
import Debug.Trace
import Data.List


-- An API for compass and straightedge constructions.
-- The API is modelled as a type class. That way we can encode different
-- semantics as different instances.

data Point
data Line
data Circle
data Points

class Monad m => MonadSpaken m r | m -> r where
  line        :: r Point  -> r Point  -> m (r Line)
  circle      :: r Point  -> r Point  -> m (r Circle)
  intersectCC :: r Circle -> r Circle -> m (r Points)
  bothPoints  :: r Points -> m (r Point, r Point)



-- An example construction: bisections of line segments.

bisection :: MonadSpaken m r => r Point -> r Point -> m (r Line)
bisection p1 p2 = do
  c1 <- circle p1 p2
  c2 <- circle p2 p1
  ps <- intersectCC c1 c2
  (i1, i2) <- bothPoints ps
  line i1 i2



-- Our example construction takes two arguments.
-- We would like to serialize constructions, even if they take arguments.
-- Those two statements tend to bite each other. To solve that, we use the
-- following GADTs to wrap constructions that expect arguments.

data ArgSpaken m r where
  -- The construction takes no more arguments. The Ix value indicates its
  -- return type.
  ArgSpaken0 :: Ix ix -> m (r ix) -> ArgSpaken m r
  
  -- The construction takes an argument whose type is indicated by the Ix.
  -- More arguments may follow.
  ArgSpaken1 :: Ix ix -> (r ix -> ArgSpaken m r) -> ArgSpaken m r



-- Ix is a GADT whose constructors are witnesses. Here is its definition, followed by some machinery.

data Ix a where
  IxPoint  :: Ix Point
  IxLine   :: Ix Line
  IxCircle :: Ix Circle
  IxPoints :: Ix Points

instance EqT Ix where
  eqT IxPoint  IxPoint  = Just Refl
  eqT IxLine   IxLine   = Just Refl
  eqT IxCircle IxCircle = Just Refl
  eqT IxPoints IxPoints = Just Refl
  eqT _        _        = undefined

data SomeIx where
  SomeIx :: Ix a -> SomeIx

instance Show SomeIx where
  show (SomeIx ix) = case ix of
    IxPoint -> "IxPoint"
    IxLine  -> "IxLine"
    IxCircle -> "IxCircle"
    IxPoints -> "IxPoints"

instance Eq SomeIx where
  SomeIx ix1 == SomeIx ix2 = case eqT ix1 ix2 of
    Just Refl -> True
    Nothing -> False

-- class    El a      where el :: Ix a
-- instance El Point  where el = IxPoint
-- instance El Line   where el = IxLine
-- instance El Circle where el = IxCircle
-- instance El Points where el = IxPoints



-- This is what a wrapped bisection looks like.

bisectionWrapped :: MonadSpaken m r => ArgSpaken m r
bisectionWrapped =
  ArgSpaken1 IxPoint $ \p1 ->
  ArgSpaken1 IxPoint $ \p2 ->
  ArgSpaken0 IxLine  $ bisection p1 p2



-- Now we are ready to serialize constructions that take arguments. A
-- serialized construction has type SerialState. The left element indicates
-- the types of the arguments the construction takes, while the right element
-- contains the individual steps that make up the complete construction. For
-- now we assume that the last step in the list produces the result of the
-- entire construction.

type SerialState = ([SomeIx], [SpakenStmt])

data SpakenStmt
  = StmtLine         Int  Int
  | StmtCircle       Int  Int
  | StmtIntersectCC  Int  Int
  | StmtBothPoints   Int
  deriving (Eq, Show)

newNamesMade :: SpakenStmt -> Int
newNamesMade (StmtBothPoints _) = 2
newNamesMade _                  = 1

-- In SerialState, most type information is lost, making it easy to convert to
-- and from text or binary. Each SpakenStmt constructor pushes some elements
-- onto the stack, as indicated by function newNamesMade. The Ints are indices
-- into this stack. The arguments are the first elements in the stack,
-- starting at index 0.

-- This is what bisection looks like when it is serialized:
-- > serialize bisectionWrapped
-- ([IxPoint,IxPoint],[StmtCircle 0 1,StmtCircle 1 0,StmtIntersectCC 3 4,StmtBothPoints 5,StmtLine 6 7])




-- We can already tell the type of the serialize function. The conversion to
-- SerialState is handled by the state monad Serialize, which is an instance
-- of MonadSpaken.

serialize :: ArgSpaken Serialize Ref -> SerialState
serialize sp = execState sp' ([], [])
  where
    Serialize sp' = serialize' sp

serialize' :: ArgSpaken Serialize Ref -> Serialize ()
serialize' (ArgSpaken0 ix sp) = sp >> return ()
  -- The result is thrown away under the assumption that the last statement's
  -- output equals the entire construction's output. This needs to be refined
  -- a bit, later on.
serialize' (ArgSpaken1 ix f) = do
  argTypes <- gets fst
  modify (first (++ [SomeIx ix]))
  serialize' . f . Ref . length $ argTypes

newtype Serialize a = Serialize (State SerialState a)
  deriving (Monad, MonadState SerialState)

instance MonadSpaken Serialize Ref where
  line        (Ref p1) (Ref p2) = andGetRef  $ addStmt (StmtLine p1 p2)
  circle      (Ref pc) (Ref pr) = andGetRef  $ addStmt (StmtCircle pc pr)
  intersectCC (Ref c1) (Ref c2) = andGetRef  $ addStmt (StmtIntersectCC c1 c2)
  bothPoints  (Ref ps)          = andGetRef2 $ addStmt (StmtBothPoints ps)

newtype Ref a = Ref { getRef :: Int }
  deriving (Eq, Show)

addStmt :: SpakenStmt -> Serialize ()
addStmt stmt = modify (second (++ [stmt]))

countNames :: Serialize Int
countNames = do
  (argTypes, stmts) <- get
  return (length argTypes + sum (map newNamesMade stmts))

andGetRef :: Serialize a -> Serialize (Ref b)
andGetRef sp = do
  n <- countNames
  sp
  return (Ref $ n + 1)

andGetRef2 :: Serialize a -> Serialize (Ref b, Ref c)
andGetRef2 sp = do
  n <- countNames
  sp
  return (Ref $ n + 1, Ref $ n + 2)



-- Now the fun part: deserialization. This is hard because all the type
-- information needs to be recovered. But we already know deserialize's type:

deserialize :: MonadSpaken m r => SerialState -> ArgSpaken m r
deserialize = deserializeAll []



-- What follows is an attempt at deserialize's implementation.

data StackEl r where
  StackEl :: Ix ix -> r ix -> StackEl r

deserializeAll :: forall m r. MonadSpaken m r =>
  [StackEl r] -> SerialState -> ArgSpaken m r
deserializeAll stack (SomeIx ty : tys, stmts) =
    ArgSpaken1 ty $ \arg ->
      deserializeAll (stack ++ [StackEl ty arg]) (tys, stmts)
deserializeAll stack ([], stmts) =
    ArgSpaken0 IxLine (getFinalStack >>= getLastEl)
  where
    getFinalStack :: m [StackEl r]
    getFinalStack = execStateT (deserializeStmts stmts) stack
    -- To do: allow return types other than Line.
    getLastEl :: [StackEl r] -> m (r Line)
    getLastEl stack = case last stack of
      StackEl IxLine l -> return l

stackIxs :: [StackEl r] -> [SomeIx]
stackIxs = map f
  where
    f (StackEl ix _) = SomeIx ix

traceShowM :: (Show a, Monad m) => a -> m ()
traceShowM x = trace (show x) (return ())

deserializeStmts :: MonadSpaken m r => [SpakenStmt] -> StateT [StackEl r] m ()
deserializeStmts [] = return ()
deserializeStmts (s:ss) = do
  traceShowM s
  stack <- get
  case s of
    StmtLine p1ref p2ref -> do
      traceShowM (p1ref, p2ref, stackIxs stack)
      case (stack !! p1ref, stack !! p2ref) of
        (StackEl IxPoint p1, StackEl IxPoint p2) -> do
          l <- lift $ line p1 p2
          modify (++ [StackEl IxLine l])
          deserializeStmts ss
        _ -> error "incompatible types"
    StmtCircle pcRef prRef -> do
      traceShowM (pcRef, prRef, stackIxs stack)
      case (stack !! pcRef, stack !! prRef) of
        (StackEl IxPoint pc, StackEl IxPoint pr) -> do
          c <- lift $ circle pc pr
          modify (++ [StackEl IxCircle c])
          deserializeStmts ss
        _ -> error "incompatible types"
    StmtIntersectCC c1ref c2ref -> do
      traceShowM (c1ref, c2ref, stackIxs stack)
      case (stack !! c1ref, stack !! c2ref) of
        (StackEl IxCircle c1, StackEl IxCircle c2) -> do
          ps <- lift $ intersectCC c1 c2
          modify (++ [StackEl IxPoints ps])
          deserializeStmts ss
        _ -> error "incompatible types"
    StmtBothPoints psRef -> do
      traceShowM (psRef, stackIxs stack)
      case stack !! psRef of
        StackEl IxPoints ps -> do
          (p1, p2) <- lift $ bothPoints ps
          modify (++ [StackEl IxPoint p1, StackEl IxPoint p2])
          deserializeStmts ss
        _ -> error "incompatible types"

propSerialiseId :: SerialState -> Bool
propSerialiseId ser = serialize (deserialize ser) == ser

test :: Bool
test = propSerialiseId (serialize bisectionWrapped)
