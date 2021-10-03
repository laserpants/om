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
import qualified Data.Text as Text

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

instance PrimType FunPrim Double where
    toPrim              = Float
    fromPrim (Float n)  = Just n
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

instance PrimType FunPrim Char where
    toPrim              = Char
    fromPrim (Char c)   = Just c
    fromPrim _          = Nothing

funPrelude :: (MonadIO m, MonadReader (EvalContext FunPrim m) m) => [(Name, m (Value FunPrim m))]
funPrelude =
    [ ("eq"      , primFun2 ((==) :: Int -> Int -> Bool))
    , ("add"     , primFun2 ((+) :: Int -> Int -> Int))
    , ("sub"     , primFun2 ((-) :: Int -> Int -> Int))
    , ("mul"     , primFun2 ((*) :: Int -> Int -> Int))
    , ("div"     , primFun2 (div :: Int -> Int -> Int))
    , ("concat"  , primFun2 ((<>) :: Text -> Text -> Text))
    , ("strcons" , primFun2 (Text.cons :: Char -> Text -> Text))
    , ("print"   , ioPutStrLn)
    , ("read"    , ioGetLine)
    ]

ioPutStrLn :: (MonadIO m, MonadReader (EvalContext FunPrim m) m) => m (Value FunPrim m)
ioPutStrLn = pure $ flip (Closure "?0") mempty $ do
    env <- ask <#> evalEnv
    fromJust (Map.lookup "?0" env) >>= \case
        Value (String str) -> do
            liftIO (putStrLn (unpack str))
            pure (Value Unit)

ioGetLine :: (MonadIO m, MonadReader (EvalContext FunPrim m) m) => m (Value FunPrim m)
ioGetLine = pure $ flip (Closure "?0") mempty $ do
    str <- liftIO getLine
    pure (Value (String (pack str)))
