{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Om.Eval.Strict
  ( ResultT
  , Result
  , PrimEnvT
  , PrimEnv
  , evalExprT
  , evalExpr
  ) where

import Control.Monad.Except
import Control.Monad.Fix
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Reader
import Data.Functor.Foldable
import Data.Maybe (fromMaybe)
import Data.Tuple.Extra (first)
import Om.Eval
import Om.Lang
import Om.Plug
import Om.Prim
import Om.Util
import qualified Data.Map.Strict as Map

type ResultT m p = Value p (EvalT m p)
type Result p = ResultT Identity p

type PrimEnvT m p = [(Name, EvalT m p (ResultT m p))]
type PrimEnv p = PrimEnvT Identity p

evalExprT
  :: (MonadFix m, PrimType p Bool)
  => Om p
  -> PrimEnvT m p
  -> Plugin p (EvalT m p)
  -> m (Either Error (ResultT m p))
evalExprT om prim plug =
    runEvalT (eval om) context
  where
    context = EvalContext
        { evalEnv = foldr (uncurry Map.insert) mempty env
        , varHook = pluginVarHook plug
        , conHook = pluginConHook plug
        , patHook = pluginPatHook plug
        }
    env = prim <#> first ("$" <>)

evalExpr
  :: (PrimType p Bool)
  => Om p
  -> PrimEnv p
  -> Plugin p (Eval p)
  -> Either Error (Result p)
evalExpr om = runIdentity <$$> evalExprT om

eval
  :: (PrimType p Bool, MonadFix m, MonadError Error m, MonadReader (EvalContext p m) m)
  => Om p
  -> m (Value p m)
eval = cata $ \case

    Var var    -> evalVar var
    Con con    -> evalCon con
    App exs    -> foldl1 evalApp exs
    Lit lit    -> pure (Value lit)
    Lam var e1 -> asks (Closure var e1 . evalEnv)

    Let var e1 e2 -> do
        e <- mfix (\val -> insertIntoEnv var val e1)
        insertIntoEnv var e e2

    If e1 e2 e3 -> do
        e <- e1
        case toBool e of
            Just isTrue ->
                if isTrue then e2 else e3

            Nothing ->
                throwError NonTruthyCondition

    Pat expr clauses ->
        evalPat expr clauses

insertIntoEnv :: (MonadReader (EvalContext p m) m) => Name -> Value p m -> m a -> m a
insertIntoEnv var val = (local  . applyEvalEnv) (Map.insert var (pure val))

toBool :: (PrimType p Bool) => Value p m -> Maybe Bool
toBool (Value v) = fromPrim v
toBool _         = Nothing

evalApp
  :: (MonadError Error m, MonadReader (EvalContext p m) m)
  => m (Value p m)
  -> m (Value p m)
  -> m (Value p m)
evalApp fx arg = do
    fun <- fx
    val <- arg
    case fun of

        Closure var body closure ->
            (local . applyEvalEnv) (Map.insert var (pure val) . (closure <>)) body

        PrimFun fun args ->
            evalPrim fun (val:args)

        Data con args ->
            pure (Data con (args <> [val]))

evalPrim
  :: (MonadError Error m, MonadReader (EvalContext p m) m)
  => Fun p
  -> [Value p m]
  -> m (Value p m)
evalPrim fun args
    | arity fun == length args = do
        vs <- traverse literal (reverse args)
        case applyFun fun vs of
            Nothing   -> throwError TypeMismatch
            Just prim -> pure (Value prim)

    | otherwise =
        pure (PrimFun fun args)

  where
    literal (Value lit) = pure lit
    literal _           = throwError RuntimeError

evalVar
  :: (MonadError Error m, MonadReader (EvalContext p m) m)
  => Name
  -> m (Value p m)
evalVar var = do
    -- Run hooks
    hook <- ask <#> varHook
    res <- hook var
    case res of
        Just v1 -> pure v1
        -- If hooks return Nothing, do regular lookup
        Nothing -> do
            val <- asks (Map.lookup var . evalEnv)
            fromMaybe (throwError (UnboundIdentifier (stripPrefix "$" var))) val

evalCon
  :: (MonadError Error m, MonadReader (EvalContext p m) m)
  => Name
  -> m (Value p m)
evalCon con = do
    hook <- ask <#> conHook
    res <- hook con
    pure $ fromMaybe (Data con []) res

evalPat
  :: (MonadError Error m, MonadReader (EvalContext p m) m)
  => m (Value p m)
  -> [(Names, m (Value p m))]
  -> m (Value p m)
evalPat val = \case

    [] -> throwError RuntimeError
    [(c, e)] | c == [wcard] -> e

    pats@((p:ps, e):eqs) -> do
        -- Run hooks
        hook <- ask <#> patHook
        res <- hook pats val
        case res of
            Just v1 -> pure v1
            -- If hooks return Nothing, continue with normal pattern matching
            Nothing -> val >>= \case

                Data con args | p == con ->
                    (local . applyEvalEnv) (insertMany (zip ps (pure <$> args))) e

                _ ->
                    evalPat val eqs
