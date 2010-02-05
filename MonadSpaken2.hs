{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MonadSpaken2 where

import Control.Monad.State
import Control.Arrow (first, second)
import Data.Type.Equality  -- from package type-equality
import Data.List


data Point
data Line
data Circle
data Points

class Monad m => MonadSpaken m r | m -> r where
  line        :: r Point  -> r Point  -> m (r Line)
  circle      :: r Point  -> r Point  -> m (r Circle)
  intersectCC :: r Circle -> r Circle -> m (r Points)
  point1      :: r Points -> m (r Point)
  point2      :: r Points -> m (r Point)


data ArgSpaken m r where
  ArgSpaken0 :: Ix ix -> m (r ix) -> ArgSpaken m r
  ArgSpaken1 :: Ix ix -> (r ix -> ArgSpaken m r) -> ArgSpaken m r


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


data AxiomType
  = AxLine
  | AxCircle
  | AxIntersectCC
  | AxPoint1
  | AxPoint2
  deriving (Eq, Show)

type AxiomInvoc = (AxiomType, [Int])

data SerialSpaken = SerialSpaken
    { serialArgTypes     :: [SomeIx]
    , serialConstruction :: [AxiomInvoc]
    , serialReturnType   :: SomeIx
    , serialReturnRef    :: Int
    }
  deriving (Eq, Show)

axioms :: MonadSpaken m r => [(AxiomType, ArgSpaken m r)]
axioms =
  [ (AxLine,
      ArgSpaken1 IxPoint  $ \p1 ->
      ArgSpaken1 IxPoint  $ \p2 ->
      ArgSpaken0 IxLine   $ line p1 p2)
  , (AxCircle,
      ArgSpaken1 IxPoint  $ \p1 ->
      ArgSpaken1 IxPoint  $ \p2 ->
      ArgSpaken0 IxCircle $ circle p1 p2)
  , (AxIntersectCC,
      ArgSpaken1 IxCircle $ \c1 ->
      ArgSpaken1 IxCircle $ \c2 ->
      ArgSpaken0 IxPoints $ intersectCC c1 c2)
  , (AxPoint1,
      ArgSpaken1 IxPoints $ \ps ->
      ArgSpaken0 IxPoint  $ point1 ps)
  , (AxPoint2,
      ArgSpaken1 IxPoints $ \ps ->
      ArgSpaken0 IxPoint  $ point2 ps)
  ]


serialize :: ArgSpaken Serialize Ref -> SerialSpaken
serialize = evalSerialize . doSerialize

doSerialize :: ArgSpaken Serialize Ref -> Serialize SerialSpaken
doSerialize arg = case arg of
  ArgSpaken0 ix sp -> do
    Ref r <- sp
    (argTypes, invocs) <- get
    return (SerialSpaken argTypes invocs (SomeIx ix) r)
  ArgSpaken1 ix f -> do
    argTypes <- gets fst
    modify (first (++ [SomeIx ix]))
    let newRef = Ref (length argTypes)
    doSerialize (f newRef)

type SerializeState = ([SomeIx], [AxiomInvoc])

newtype Serialize a = Serialize (State SerializeState a)
  deriving (Monad, MonadState SerializeState)

evalSerialize :: Serialize a -> a
evalSerialize (Serialize st) = evalState st ([], [])

instance MonadSpaken Serialize Ref where
  line        (Ref p1) (Ref p2) = mkInvoke (AxLine,         [p1, p2])
  circle      (Ref pc) (Ref pr) = mkInvoke (AxCircle,       [pc, pr])
  intersectCC (Ref c1) (Ref c2) = mkInvoke (AxIntersectCC,  [c1, c2])
  point1      (Ref ps)          = mkInvoke (AxPoint1,       [ps])
  point2      (Ref ps)          = mkInvoke (AxPoint2,       [ps])

mkInvoke :: AxiomInvoc -> Serialize (Ref a)
mkInvoke invoc = do
  (argTypes, invocs) <- get
  let newRef = Ref (length argTypes + length invocs)
  modify (second (++ [invoc]))
  return newRef


newtype Ref a = Ref { getRef :: Int }
  deriving (Eq, Show)


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
        finalStack <- execStateT (mapM_ deserializeStmt stmts) stack
        case finalStack !! retRef of
          StackEl ty' value -> return (coerceRetTy ty' retTy value)

coerceRetTy :: Ix ix1 -> Ix ix2 -> r ix1 -> r ix2
coerceRetTy ty1 ty2 = case eqT ty1 ty2 of
  Just Refl -> id
  Nothing   -> error $
      "Declared return type " ++ show (SomeIx ty2) ++
      " does not match computed return type " ++ show (SomeIx ty1)

deserializeStmt :: MonadSpaken m r => AxiomInvoc -> StateT [StackEl r] m ()
deserializeStmt (ax, args) = do
  let Just poly = ax `lookup` axioms
  invoke poly args

invoke :: MonadSpaken m r => ArgSpaken m r -> [Int] -> StateT [StackEl r] m ()
invoke (ArgSpaken0 retIx ac) [] = do
  res <- lift ac
  modify (++ [StackEl retIx res])
invoke (ArgSpaken1 argIx ac) (r:rs) = do
  stack <- get
  case stack !! r of
    StackEl stackIx el -> bla argIx ac stackIx el rs
invoke _ _ = error "argument count mismatch"

bla :: MonadSpaken m r => Ix argIx -> (r argIx -> ArgSpaken m r) -> Ix stackIx -> r stackIx -> [Int] -> StateT [StackEl r] m ()
bla argIx ac stackIx el rs =
  case eqT argIx stackIx of
    Just Refl -> invoke (ac el) rs
    Nothing   -> error "type mismatch"



bisection :: MonadSpaken m r => r Point -> r Point -> m (r Line)
bisection p1 p2 = do
  c1 <- circle p1 p2
  c2 <- circle p2 p1
  ps <- intersectCC c1 c2
  i1 <- point1 ps
  i2 <- point2 ps
  line i1 i2

bisectionWrapped :: MonadSpaken m r => ArgSpaken m r
bisectionWrapped =
  ArgSpaken1 IxPoint $ \p1 ->
  ArgSpaken1 IxPoint $ \p2 ->
  ArgSpaken0 IxLine  $ bisection p1 p2

unwrapBisection :: MonadSpaken m r => ArgSpaken m r ->
  r Point -> r Point -> m (r Line)
unwrapBisection arg p1 p2 =  case arg of
  ArgSpaken1 IxPoint f    -> case f p1 of
    ArgSpaken1 IxPoint g  -> case g p2 of
      ArgSpaken0 IxLine l -> l

propSerialiseId :: SerialSpaken -> Bool
propSerialiseId ser = serialize (deserialize ser) == ser

test :: Bool
test = propSerialiseId (serialize bisectionWrapped)

