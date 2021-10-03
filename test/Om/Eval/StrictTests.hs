{-# LANGUAGE OverloadedStrings #-}
module Om.Eval.StrictTests where

import Data
import Data.Text (Text, unpack, pack)
import Om.Eval
import Om.Eval.Strict
import Om.Lang
import Om.Prim.Basic
import Test.Hspec
import qualified Om.Prim.Basic as Basic
import qualified Om.Prim.Basic.Parser as Basic

evalTests :: SpecWith ()
evalTests = do

    describe "Eval" $ do

        testEvalStrict "8 factorial [40320]"
            --
            --  let
            --    fact =
            --      n =>
            --        if $eq(n, 0)
            --          then
            --            1
            --          else
            --            $mul(n, fact($sub(n, 1)))
            --    in
            --      fact(8)
            --
            (omLet "fact"
                (omLam "n"
                    (omIf
                        (omApp [omPrim "eq", omVar "n", omLit (Basic.Int 0)])
                        (omLit (Basic.Int 1))
                        (omApp
                            [ omPrim "mul"
                            , omVar "n"
                            , omApp
                                [ omVar "fact"
                                , omApp
                                    [ omPrim "sub"
                                    , omVar "n"
                                    , omLit (Basic.Int 1)
                                    ]
                                ]
                            ])))
                    (omApp [omVar "fact", omLit (Basic.Int 8)]))
            (Right (Value (Basic.Int 40320)))

        testEvalStrict "Cons(1, Cons(2, (Cons 3, Nil)))"
            example1
            (Right (Data "Cons" [Value (Basic.Int 1), Data "Cons" [Value (Basic.Int 2), Data "Cons" [Value (Basic.Int 3), Data "Nil" []]]]))

        testEvalStrict "Cons(1, Cons(2, (Cons 3, Nil)))"
            (omData "Cons" [omLit (Basic.Int 1), omData "Cons" [omLit (Basic.Int 2), omData "Cons" [omLit (Basic.Int 3), omData "Nil" []]]])
            (Right (Data "Cons" [Value (Basic.Int 1), Data "Cons" [Value (Basic.Int 2), Data "Cons" [Value (Basic.Int 3), Data "Nil" []]]]))

        testEvalStrict "match Cons(1, Cons(2, Cons (3, Nil))) | Cons(n, _) = n [1]"
            --
            -- match Cons(1, Cons(2, (Cons 3, Nil)))
            --   | Cons(n, _) = n
            --
            (omPat example1 [ (["Cons", "n", wcard], omVar "n") ])
            (Right (Value (Basic.Int 1)))

        testEvalStrict "match Nil | Cons(n, _) = n | Nil = 100 [100]"
            --
            -- match Nil
            --   | Cons(n, _) = n
            --   | Nil = 100
            --
            (omPat (omData "Nil" []) [ (["Cons", "n", wcard], omVar "n") , (["Nil"], omLit (Basic.Int 100)) ])
            (Right (Value (Basic.Int 100)))

        testEvalStrict "match Cons(1, Cons(2, Cons (3, Nil))) | Cons(_, xs) = match xs | Cons(n, _) = n end | Nil = 100 [2]"
            --
            -- match Cons(1, Cons(2, Cons (3, Nil)))
            --   | Cons(_, xs) =
            --       match xs
            --         | Cons(n, _) = n
            --         end
            --   | Nil = 100
            --
            (omPat example1
                [ (["Cons", wcard, "xs"],
                    omPat (omVar "xs")
                        [ (["Cons", "n", wcard], omVar "n") ])
                , (["Nil"], omLit (Basic.Int 100))
                ])
            (Right (Value (Basic.Int 2)))

    describe "Eval (failures)" $ do

        testEvalStrict "x"
            (omVar "x")
            (Left (UnboundIdentifier "x"))

        testEvalStrict "X"
            (omData "X" [])
            (Right (Data "X" []))

        testEvalStrict "X(1)"
            (omData "X" [omLit (Basic.Int 1)])
            (Right (Data "X" [Value (Basic.Int 1)]))

        testEvalStrict "if 5 then 1 else 2"
            (omIf (omLit (Basic.Int 5))
                (omLit (Basic.Int 1))
                (omLit (Basic.Int 2)))
            (Left NonTruthyCondition)

testEvalStrict :: Text -> Om BasicPrim -> Either Error (ResultT IO BasicPrim) -> SpecWith ()
testEvalStrict dscr om expect = do
    result <- runIO (evalExprT om basicPrelude mempty)
    it (unpack dscr) (expect == result)
