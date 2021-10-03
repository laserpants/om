{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Om.Plug.Records
  ( recordsPlugin
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Foldable
import Data.Map.Strict (Map)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Om.Eval
import Om.Plug
import Om.Util
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

recordsPlugin :: (Monad m) => Plugin p (EvalT m p)
recordsPlugin = plugin (Just recordsVarHook) Nothing (Just recordsPatHook)

recordsVarHook :: (Monad m) => VarHook p (EvalT m p)
recordsVarHook var
  | '.' == Text.head var = do
      let closr body = pure (Closure "?0" body mempty)
      Just <$$> closr $ do
          env <- ask <#> evalEnv
          fromJust (Map.lookup "?0" env) >>= \case

              Data "#" fields ->
                  getField (stripPrefix "." var) fields
              _ ->
                  throwError RuntimeError

  | "#" == var || '{' == Text.head var =
      pure (Just (Data var []))

  | otherwise = pure Nothing

getField :: (Monad m) => Name -> [Value p m] -> m (Value p m)
getField name [Data f (v:fs)]
  | f == ("{" <> name <> "}") = pure v
  | otherwise                 = getField name fs

recordsPatHook :: (Monad m) => PatHook p (EvalT m p)
recordsPatHook (([p, q, r], e):_) val
  | isRowCon p = do
        fun <- do
            row <- toMap val mempty
            case Map.lookup p row of
                Nothing    -> throwError RuntimeError
                Just (v:_) -> pure (insertMany [(q, v), (r, fromMap (Map.delete p row))])
        Just <$> local (applyEvalEnv fun) e

recordsPatHook _ _ = pure Nothing

isRowCon :: Name -> Bool
isRowCon ""  = False
isRowCon con = Text.head con == '{' && Text.last con == '}'

toMap :: (Monad m) => m (Value p m) -> Map Name [m (Value p m)] -> m (Map Name [m (Value p m)])
toMap val row = val >>= \case

    Data "{}" []    -> pure row
    Data lab (v:vs) -> foldrM toMap (Map.insertWith (<>) lab [pure v] row) (pure <$> vs)

fromMap :: (Monad m) => Map Name [m (Value p m)] -> m (Value p m)
fromMap row = foldrM (uncurry fun) (Data "{}" []) (Map.toList row)
  where
    fun con = flip $ foldrM $ \val next -> do
        this <- val
        pure (Data con [this, next])
