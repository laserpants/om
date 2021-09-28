{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec

import Data.Char (isUpper)
import Data.Text (Text, unpack)
import Om.Eval
import Om.Eval.Strict
import Om.Lang
import Om.Prim
import Om.Prim.Basic
import Om.Util
import qualified Data.Text as Text

instance (Eq p) => Eq (Value p m) where
    (==) (Value p) (Value q) = p == q
    (==) (Data c ps) (Data d qs) = c == d && ps == qs
    (==) _ _ = False

hasUpperInitial :: Name -> Bool
hasUpperInitial "" = False
hasUpperInitial name = isUpper (Text.head name)

-- Middleware?
varHook :: Name -> Eval BasicPrim (Maybe (Result BasicPrim))
varHook var
  | hasUpperInitial var = pure (Just (Data var []))
  | otherwise = pure Nothing

testEval :: Text -> Om BasicPrim -> Either Error (Result BasicPrim) -> SpecWith ()
testEval dscr om exp =
    it (unpack dscr) (exp == res)
  where
    res = evalExpr om basicPrelude varHook

------------------------------------------------------------------------------------------------------

example1 :: Om BasicPrim
example1 =
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

------------------------------------------------------------------------------------------------------

main :: IO ()
main = do
    hspec $ do

        describe "Eval" $ do

------------------------------------------------------------------------------------------------------

            testEval "8 factorial"
                --
                --  let
                --    fact =
                --      (n) =>
                --        if .eq(n, 0)
                --          then 1
                --          else .mul(n, fact(.sub(n, 1)))
                --    in
                --      fact(8)
                --
                (omLet "fact"
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
                        (omApp [omVar "fact", omLit (PInt 8)]))
                    (Right (Value (PInt 40320)))

------------------------------------------------------------------------------------------------------

            testEval "Cons(1, Cons(2, Nil))"
                example1
                (Right (Data "Cons" [Value (PInt 1), Data "Cons" [Value (PInt 2), Data "Cons" [Value (PInt 3), Data "Nil" []]]]))

------------------------------------------------------------------------------------------------------

            testEval "match Cons(1, Cons(2, Nil)) with | Cons(n, _) = n"
                --
                -- match Cons(1, Cons(2, Nil)) with
                --   | Cons(n, _) = n
                --
                (omPat example1 [ (["Cons", "n", wcard], omVar "n") ])
                (Right (Value (PInt 1)))

------------------------------------------------------------------------------------------------------

            testEval "match Nil with | Cons(n, _) = n | Nil = 100"
                --
                -- match Nil with
                --   | Cons(n, _) = n
                --   | Nil = 100
                --
                (omPat (omVar "Nil") [ (["Cons", "n", wcard], omVar "n") , (["Nil"], omLit (PInt 100)) ])
                (Right (Value (PInt 100)))

------------------------------------------------------------------------------------------------------

            testEval "match Cons(1, Cons(2, Nil)) with | Cons(_, xs) = match xs with | Cons(n, _) = n | Nil = 100"
                --
                -- match Cons(1, Cons(2, Nil)) with
                --   | Cons(_, xs) =
                --       match xs with
                --         | Cons(n, _) = n
                --   | Nil = 100
                --
                (omPat example1
                    [ (["Cons", wcard, "xs"],
                        (omPat (omVar "xs")
                            [ (["Cons", "n", wcard], omVar "n") ]))
                    , (["Nil"], omLit (PInt 100))
                    ])
                (Right (Value (PInt 2)))
