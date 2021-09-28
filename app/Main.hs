{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Data.Char (isUpper)
import Om.Eval.Strict
import Om.Lang
import Om.Util
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

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

hasUpperInitial :: Name -> Bool
hasUpperInitial "" = False
hasUpperInitial name = isUpper (Text.head name)

varHook :: Name -> Eval Prim (Maybe (Result Prim))
varHook var
  | hasUpperInitial var = pure (Just (Data var []))
  | otherwise = pure Nothing

-- let
--   fact =
--     (n) =>
--       if .eq(n, 0)
--         then 1
--         else .mul(n, fact(.sub(n, 1)))
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
test1 = evalExpr test1Expr env varHook <#> toString

-- Cons(1, Cons(2, Nil))
--
test2Expr :: Om Prim
test2Expr =
    omData "Cons"
        [ omLit (PInt 1)
        , omData "Cons"
            [ omLit (PInt 2)
            , omData "Cons"
                [ omLit (PInt 3)
                , omData "Nil" []
                ]
            ]
        ]

test2 :: Either Error String
test2 = evalExpr test2Expr env varHook <#> toString

-- match Cons(1, Cons(2, Nil)) with
--   | Cons(n, _) = n
--
test3Expr :: Om Prim
test3Expr =
    omPat test2Expr
        [ (["Cons", "n", wcard], omVar "n") ]

test3 :: Either Error String
test3 = evalExpr test3Expr env varHook <#> toString

-- match Nil with
--   | Cons(n, _) = n
--   | Nil = 100
--
test4Expr :: Om Prim
test4Expr =
    omPat (omVar "Nil")
        [ (["Cons", "n", wcard], omVar "n")
        , (["Nil"], omLit (PInt 100))
        ]

test4 :: Either Error String
test4 = evalExpr test4Expr env varHook <#> toString

-- match Cons(1, Cons(2, Nil)) with
--   | Cons(_, xs) =
--       match xs with
--         | Cons(n, _) = n
--   | Nil = 100
--
test5Expr :: Om Prim
test5Expr =
    omPat test2Expr
        [ (["Cons", wcard, "xs"],
            (omPat (omVar "xs")
                [ (["Cons", "n", wcard], omVar "n") ]))
        , (["Nil"], omLit (PInt 100))
        ]

test5 :: Either Error String
test5 = evalExpr test5Expr env varHook <#> toString

main :: IO ()
main = print "OK"
