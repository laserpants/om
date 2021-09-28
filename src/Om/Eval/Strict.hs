{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StrictData                 #-}
module Om.Eval.Strict where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Functor.Foldable
import Data.List (intersperse)
import Data.Map.Strict (Map)
import Data.Text (unpack)
import Data.Tuple.Extra (first)
import Om.Lang
import Om.Util
import qualified Data.Map.Strict as Map

class PrimType p t where
    toPrim   :: t -> p
    fromPrim :: p -> Maybe t

data Value p m
    = Value p
    | Data Name [Value p m]
    | Closure Name (m (Value p m)) (EvalEnv p m)
    | PrimFun (Fun p) [Value p m]

toString :: (Show p) => Value p m -> String
toString = \case
    Value p      -> show p
    Data con vs  -> unpack con <> " " <> flatten (toString <$> vs)
    Closure{}    -> "((function))"
    _            -> "((internal))"
  where
    flatten strs = "[" <> foldr (<>) "" (intersperse ", " strs) <> "]"

data Fun p
    = Fun1 (p -> Maybe p)
    | Fun2 (p -> p -> Maybe p)
    | Fun3 (p -> p -> p -> Maybe p)
    | Fun4 (p -> p -> p -> p -> Maybe p)
    | Fun5 (p -> p -> p -> p -> p -> Maybe p)

fun1 :: (PrimType p a, PrimType p b) => (a -> b) -> Fun p
fun1 f = Fun1 (\a -> let b = f <$> fromPrim a in toPrim <$> b)

fun2 :: (PrimType p a, PrimType p b, PrimType p c) => (a -> b -> c) -> Fun p
fun2 f = Fun2 (\a b -> let c = f <$> fromPrim a <*> fromPrim b in toPrim <$> c)

fun3 :: (PrimType p a, PrimType p b, PrimType p c, PrimType p d) => (a -> b -> c -> d) -> Fun p
fun3 f = Fun3 (\a b c -> let d = f <$> fromPrim a <*> fromPrim b <*> fromPrim c in toPrim <$> d)

fun4 :: (PrimType p a, PrimType p b, PrimType p c, PrimType p d, PrimType p e) => (a -> b -> c -> d -> e) -> Fun p
fun4 f = Fun4 (\a b c d -> let e = f <$> fromPrim a <*> fromPrim b <*> fromPrim c <*> fromPrim d in toPrim <$> e)

fun5 :: (PrimType p a, PrimType p b, PrimType p c, PrimType p d, PrimType p e, PrimType p f) => (a -> b -> c -> d -> e -> f) -> Fun p
fun5 f = Fun5 (\a b c d e -> let g = f <$> fromPrim a <*> fromPrim b <*> fromPrim c <*> fromPrim d <*> fromPrim e in toPrim <$> g)

arity :: Fun p -> Int
arity = \case
    Fun1 _ -> 1
    Fun2 _ -> 2
    Fun3 _ -> 3
    Fun4 _ -> 4
    Fun5 _ -> 5

applyFun :: Fun p -> [p] -> Maybe p
applyFun fun args =
    case fun of
        Fun1 f -> f (head args)
        Fun2 f -> f (head args) (args !! 1)
        Fun3 f -> f (head args) (args !! 1) (args !! 2)
        Fun4 f -> f (head args) (args !! 1) (args !! 2) (args !! 3)
        Fun5 f -> f (head args) (args !! 1) (args !! 2) (args !! 3) (args !! 4)

type EvalEnv p m = Map Name (m (Value p m))

data Error
    = UnboundIdentifier Name
    | NonTruthyCondition
    | TypeMismatch
    | RuntimeError
    deriving (Show)

type Hooks p m = Name -> m (Maybe (Value p m))

newtype Eval p a = Eval { unEval :: ReaderT (EvalEnv p (Eval p), Hooks p (Eval p)) (Either Error) a }
    deriving
      ( Functor
      , Applicative
      , Monad
      , MonadError Error
      , MonadReader (EvalEnv p (Eval p), Hooks p (Eval p)) )

runEval :: Eval p a -> (EvalEnv p (Eval p), Hooks p (Eval p)) -> Either Error a
runEval = runReaderT . unEval

type Result p = Value p (Eval p)

type PrimEnv p = [(Name, Eval p (Result p))]

evalExpr :: (PrimType p Bool) => Om p -> PrimEnv p -> Hooks p (Eval p) -> Either Error (Result p)
evalExpr om funs lup = runEval (eval om) (foldr (uncurry Map.insert) mempty env, lup)
  where
    env = funs <#> first ("$" <>)

primFun1 :: (Monad m) => (PrimType p a, PrimType p b) => (a -> b) -> m (Value p m)
primFun1 f = pure $ PrimFun (fun1 f) []

primFun2 :: (Monad m) => (PrimType p a, PrimType p b, PrimType p c) => (a -> b -> c) -> m (Value p m)
primFun2 f = pure $ PrimFun (fun2 f) []

primFun3 :: (Monad m) => (PrimType p a, PrimType p b, PrimType p c, PrimType p d) => (a -> b -> c -> d) -> m (Value p m)
primFun3 f = pure $ PrimFun (fun3 f) []

primFun4 :: (Monad m) => (PrimType p a, PrimType p b, PrimType p c, PrimType p d, PrimType p e) => (a -> b -> c -> d -> e) -> m (Value p m)
primFun4 f = pure $ PrimFun (fun4 f) []

primFun5 :: (Monad m) => (PrimType p a, PrimType p b, PrimType p c, PrimType p d, PrimType p e, PrimType p f) => (a -> b -> c -> d -> e -> f) -> m (Value p m)
primFun5 f = pure $ PrimFun (fun5 f) []

primValue :: (Monad m) => p -> m (Value p m)
primValue p = pure (Value p)

primData :: (Monad m) => Name -> [Value p m] -> m (Value p m)
primData con vs = pure (Data con vs)

eval
  :: (PrimType p Bool, MonadError Error m, MonadReader (EvalEnv p m, Hooks p m) m)
  => Om p
  -> m (Value p m)
eval = cata $ \case

    Var var    -> evalVar var
    App exs    -> foldl1 evalApp exs
    Lit lit    -> pure (Value lit)
    Lam var e1 -> asks (Closure var e1 . fst)

    Let var e1 e2 -> do
        e <- e1
        (local . first) (Map.insert var (pure e)) e2

    If e1 e2 e3 -> do
        e <- e1
        case toBool e of
            Just isTrue ->
                if isTrue then e2 else e3

            Nothing ->
                throwError NonTruthyCondition

    Pat expr clauses ->
        evalPat expr clauses 

toBool :: (PrimType p Bool) => Value p m -> Maybe Bool
toBool (Value v) = fromPrim v
toBool _         = Nothing

evalApp
  :: (MonadError Error m, MonadReader (EvalEnv p m, Hooks p m) m)
  => m (Value p m)
  -> m (Value p m)
  -> m (Value p m)
evalApp fx arg = do
    fun <- fx
    val <- arg
    case fun of

        Closure var body closure ->
            (local . first) (Map.insert var (pure val) . (closure <>)) body

        PrimFun fun args ->
            evalPrim fun (val:args)

        Data con args ->
            pure (Data con (args <> [val]))

evalPrim
  :: (MonadError Error m, MonadReader (EvalEnv p m, Hooks p m) m)
  => Fun p
  -> [Value p m]
  -> m (Value p m)
evalPrim fun args
    | arity fun == length args = do
        xs <- traverse literal (reverse args)
        case applyFun fun xs of
            Nothing   -> throwError TypeMismatch
            Just prim -> pure (Value prim)

    | otherwise =
        pure (PrimFun fun args)

  where
    literal (Value lit) = pure lit
    literal _           = throwError RuntimeError

evalVar
  :: (MonadError Error m, MonadReader (EvalEnv p m, Hooks p m) m)
  => Name
  -> m (Value p m)
evalVar var = do
    hook <- ask <#> snd
    val1 <- hook var
    case val1 of
        Just val -> pure val
        Nothing -> do
            val2 <- asks (Map.lookup var . fst)
            case val2 of
                Just val -> val
                Nothing  -> throwError (UnboundIdentifier var)

evalPat
  :: (MonadError Error m, MonadReader (EvalEnv p m, Hooks p m) m)
  => m (Value p m) 
  -> OMatrix (m (Value p m)) 
  -> m (Value p m)
evalPat val = \case

    [] -> throwError RuntimeError
    [(c, e)] | c == [wcard] -> e

    ((p:ps, e):eqs) ->
        val >>= \case

            Data con args | p == con ->
                (local . first) (insertMany (zip ps (pure <$> args))) e

            _ ->
                evalPat val eqs 
