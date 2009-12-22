{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
  eqT _        _        = Nothing

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

data SerialSpaken = SerialSpaken
    { serialArgTypes     :: [SomeIx]
    , serialConstruction :: [SpakenStmt]
    , serialReturnType   :: SomeIx
    , serialReturnRef    :: Int
    }
  deriving (Eq, Show)

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
-- SerialSpaken
--   { serialArgTypes     = [IxPoint,IxPoint]
--   , serialConstruction = [StmtCircle 0 1,StmtCircle 1 0,
--         StmtIntersectCC 2 3,StmtBothPoints 4,StmtLine 5 6]
--   , serialReturnType   = IxLine
--   , serialReturnRef    = 7
--   }





-- We can already tell the type of the serialize function. The conversion to
-- SerialState is handled by the state monad Serialize, which is an instance
-- of MonadSpaken.

serialize :: ArgSpaken Serialize Ref -> SerialSpaken
serialize sp = evalState sp' ([], [])
  where
    Serialize sp' = serialize' sp

serialize' :: ArgSpaken Serialize Ref -> Serialize SerialSpaken
serialize' arg = case arg of
  ArgSpaken0 ix sp -> do
    Ref r <- sp
    (argTypes, stmts) <- get
    return (SerialSpaken argTypes stmts (SomeIx ix) r)
  ArgSpaken1 ix f -> do
    argTypes <- gets fst
    modify (first (++ [SomeIx ix]))
    serialize' . f . Ref . length $ argTypes

type SerializeState = ([SomeIx], [SpakenStmt])

newtype Serialize a = Serialize (State SerializeState a)
  deriving (Monad, MonadState SerializeState)

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
  return (Ref n)

andGetRef2 :: Serialize a -> Serialize (Ref b, Ref c)
andGetRef2 sp = do
  n <- countNames
  sp
  return (Ref n, Ref $ n + 1)



-- Now the fun part: deserialization. This is hard because all the type
-- information needs to be recovered. But we already know deserialize's type:

deserialize :: MonadSpaken m r => SerialSpaken -> ArgSpaken m r
deserialize = deserializeAll []

data StackEl r where
  StackEl :: Ix ix -> r ix -> StackEl r

deserializeAll :: MonadSpaken m r =>
  [StackEl r] -> SerialSpaken -> ArgSpaken m r
deserializeAll stack
    serial@(SerialSpaken argTypes stmts (SomeIx retTy) retRef) =
  case argTypes of
    SomeIx ty : tys ->
      -- Push argument on stack
      ArgSpaken1 ty $ \arg ->
        deserializeAll
            (stack ++ [StackEl ty arg])
            (serial { serialArgTypes = tys })
    [] ->
      -- All arguments have been processed. Process statements.
      ArgSpaken0 retTy $ do
        finalStack <- execStateT (deserializeStmts stmts) stack
        case finalStack !! retRef of
          StackEl ty' value -> return (coerceRetTy ty' retTy value)

coerceRetTy :: Ix ix1 -> Ix ix2 -> r ix1 -> r ix2
coerceRetTy ty1 ty2 = case eqT ty1 ty2 of
  Just Refl -> id
  Nothing   -> fail $
      "Declared return type " ++ show (SomeIx ty2) ++
      " does not match computed return type " ++ show (SomeIx ty1)

deserializeStmts :: MonadSpaken m r => [SpakenStmt] -> StateT [StackEl r] m ()
deserializeStmts [] = return ()
deserializeStmts (s:ss) = do
  stack <- get
  case s of
    StmtLine p1ref p2ref -> do
      case (stack !! p1ref, stack !! p2ref) of
        (StackEl IxPoint p1, StackEl IxPoint p2) -> do
          l <- lift $ line p1 p2
          modify (++ [StackEl IxLine l])
          deserializeStmts ss
        _ -> error "incompatible types"
    StmtCircle pcRef prRef -> do
      case (stack !! pcRef, stack !! prRef) of
        (StackEl IxPoint pc, StackEl IxPoint pr) -> do
          c <- lift $ circle pc pr
          modify (++ [StackEl IxCircle c])
          deserializeStmts ss
        _ -> error "incompatible types"
    StmtIntersectCC c1ref c2ref -> do
      case (stack !! c1ref, stack !! c2ref) of
        (StackEl IxCircle c1, StackEl IxCircle c2) -> do
          ps <- lift $ intersectCC c1 c2
          modify (++ [StackEl IxPoints ps])
          deserializeStmts ss
        _ -> error "incompatible types"
    StmtBothPoints psRef -> do
      case stack !! psRef of
        StackEl IxPoints ps -> do
          (p1, p2) <- lift $ bothPoints ps
          modify (++ [StackEl IxPoint p1, StackEl IxPoint p2])
          deserializeStmts ss
        _ -> error "incompatible types"

propSerialiseId :: SerialSpaken -> Bool
propSerialiseId ser = serialize (deserialize ser) == ser

test :: Bool
test = propSerialiseId (serialize bisectionWrapped)
