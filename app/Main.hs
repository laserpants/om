{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Om.Eval.Strict
import Om.Lang
import Om.Util
import qualified Data.Map.Strict as Map

data Prim = PInt Int | PBool Bool
    deriving (Show)

instance PrimType Prim Int where
    toPrim n           = PInt n
    fromPrim (PInt n)  = Just n
    fromPrim _         = Nothing

instance PrimType Prim Bool where
    toPrim b           = PBool b
    fromPrim (PBool b) = Just b
    fromPrim _         = Nothing

env :: PrimEnv Prim
env =
    [ ("eq"  , primFun2 ((\a b -> a == b) :: Int -> Int -> Bool))
    , ("add" , primFun2 ((\a b -> a + b) :: Int -> Int -> Int ))
    , ("sub" , primFun2 ((\a b -> a - b) :: Int -> Int -> Int ))
    , ("mul" , primFun2 ((\a b -> a * b) :: Int -> Int -> Int ))
    ]

-- let
--   fact =
--     (n =>
--       if $eq(n, 0)
--         then 1
--         else $mul(n, fact($sub(n, 1))))
--   in
--     fact(8)
--
test1Expr :: Om Prim
test1Expr =
    omLet "fact"
        (omLam "n"
            (omIf
                (omApp [omPfx "eq", omVar "n", omLit (PInt 0)])
                (omLit (PInt 1))
                (omApp
                    [ omPfx "mul"
                    , omVar "n"
                    , omApp
                        [ omVar "fact"
                        , omApp
                            [ omPfx "sub"
                            , omVar "n"
                            , omLit (PInt 1)
                            ]
                        ]
                    ])))
        (omApp [omVar "fact", omLit (PInt 8)])

test1 :: Either Error String
test1 = evalExpr test1Expr env <#> toString

main :: IO ()
main = print "OK"
