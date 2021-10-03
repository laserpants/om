{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}
module Om.Prim.FunLang
  ( FunPrim(..)
  , funPrelude
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Maybe (fromMaybe, fromJust)
import Data.Text (Text, pack, unpack)
import Debug.Trace
import Om.Eval
import Om.Prim
import Om.Util
import System.IO.Unsafe
import qualified Data.Map.Strict as Map

data FunPrim
    = Unit
    | Bool Bool
    | Int Int
    | Float Double
    | Char Char
    | String Text
    deriving (Show, Eq)

instance PrimType FunPrim Int where
    toPrim              = Int
    fromPrim (Int n)    = Just n
    fromPrim _          = Nothing

instance PrimType FunPrim Bool where
    toPrim              = Bool
    fromPrim (Bool b)   = Just b
    fromPrim _          = Nothing

instance PrimType FunPrim () where
    toPrim _            = Unit
    fromPrim Unit       = Just ()
    fromPrim _          = Nothing

instance PrimType FunPrim Text where
    toPrim              = String
    fromPrim (String t) = Just t
    fromPrim _          = Nothing

instance (PrimType FunPrim a) => PrimType FunPrim (IO a) where
    toPrim              = toPrim . unsafePerformIO
    fromPrim            = fmap pure . fromPrim

funPrelude :: (MonadReader (EvalContext FunPrim m) m) => [(Name, m (Value FunPrim m))]
funPrelude =
    [ ("eq"    , primFun2 ((==) :: Int -> Int -> Bool))
    , ("add"   , primFun2 ((+) :: Int -> Int -> Int ))
    , ("sub"   , primFun2 ((-) :: Int -> Int -> Int ))
    , ("mul"   , primFun2 ((*) :: Int -> Int -> Int ))
    , ("div"   , primFun2 (div :: Int -> Int -> Int ))
    , ("print" , print_)
    , ("read"  , read_)
    ]

print_ :: (MonadReader (EvalContext FunPrim m) m) => m (Value FunPrim m)
print_ = pure $ flip (Closure "?0") mempty $ do
    env <- ask <#> evalEnv
    fromJust (Map.lookup "?0" env) >>= \case
        Value (String str) ->
            seq (unsafePerformIO (putStrLn (unpack str))) $ pure (Value Unit)

read_ :: (MonadReader (EvalContext FunPrim m) m) => m (Value FunPrim m)
read_ = pure $ flip (Closure "?0") mempty $ do
    let str = unsafePerformIO getLine 
    pure (Value (String (pack str)))
    --env <- ask <#> evalEnv
    --fromJust (Map.lookup "?0" env) >>= \case
    --    Value (String str) ->
    --        seq (unsafePerformIO (putStrLn (unpack str))) $ pure (Value Unit)

