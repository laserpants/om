{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Om.Eval.Strict where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Functor.Foldable
import Data.Tuple.Extra (first)
import Om.Eval
import Om.Lang
import Om.Prim
import Om.Util
import qualified Data.Map.Strict as Map

type Result p = Value p (Eval p)

type PrimEnv p = [(Name, Eval p (Result p))]

evalExpr :: (PrimType p Bool) => Om p -> PrimEnv p -> Hooks p (Eval p) -> Either Error (Result p)
evalExpr om funs lup = runEval (eval om) (foldr (uncurry Map.insert) mempty env, lup)
  where
    env = funs <#> first ("$" <>)

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
  -> [(Names, m (Value p m))]
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
