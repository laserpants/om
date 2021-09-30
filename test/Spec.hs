{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec

import Control.Monad.Except
import Control.Monad.Reader
import Data.Text (Text, unpack)
import Om.Eval
import Om.Eval.Strict
import Om.Lang
import Om.Plug
import Om.Plug.Constructor
import Om.Plug.Pattern
import Om.Prim
import Om.Prim.Basic
import Om.Util
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

testEval :: Text -> Om BasicPrim -> Either Error (Result BasicPrim) -> SpecWith ()
testEval dscr om expect =
    it (unpack dscr) (expect == result)
  where
    result = evalExpr om basicPrelude (constructorPlugin <> recordPlugin)

------------------------------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
    evalTests
    evalRecordsTests

------------------------------------------------------------------------------------------------------

-- Cons(1, Cons(2, Nil))
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

-- #({one}(1, {two}(2, {})))
--
-- { one = 1, two = 2 }
example2 :: Om BasicPrim
example2 =
    omData "#"
        [ omData "{one}"
            [ omLit (PInt 1)
            , omData "{two}"
                [ omLit (PInt 2)
                , omData "{}" []
                ]
            ]
        ]

-- #({two}(2, {one}(1, {})))
--
-- { two = 2, one = 1 }
example3 :: Om BasicPrim
example3 =
    omData "#"
        [ omData "{two}"
            [ omLit (PInt 2)
            , omData "{one}"
                [ omLit (PInt 1)
                , omData "{}" []
                ]
            ]
        ]

-- #({one}(1, {}))
--
-- { one = 1 }
example4 :: Om BasicPrim
example4 =
    omData "#"
        [ omData "{one}"
            [ omLit (PInt 1)
            , omData "{}" [] 
            ]
        ]

------------------------------------------------------------------------------------------------------

evalTests :: SpecWith ()
evalTests =

    describe "Eval" $ do

        testEval "8 factorial [40320]"
            --
            --  let
            --    fact =
            --      n =>
            --        if @eq(n, 0)
            --          then 1
            --          else @mul(n, fact(@sub(n, 1)))
            --    in
            --      fact(8)
            --
            (omLet "fact"
                (omLam "n"
                    (omIf
                        (omApp [omPrim "eq", omVar "n", omLit (PInt 0)])
                        (omLit (PInt 1))
                        (omApp
                            [ omPrim "mul"
                            , omVar "n"
                            , omApp
                                [ omVar "fact"
                                , omApp
                                    [ omPrim "sub"
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

        testEval "match Cons(1, Cons(2, Nil)) with | Cons(n, _) = n [1]"
            --
            -- match Cons(1, Cons(2, Nil)) with
            --   | Cons(n, _) = n
            --
            (omPat example1 [ (["Cons", "n", wcard], omVar "n") ])
            (Right (Value (PInt 1)))

------------------------------------------------------------------------------------------------------

        testEval "match Nil with | Cons(n, _) = n | Nil = 100 [100]"
            --
            -- match Nil with
            --   | Cons(n, _) = n
            --   | Nil = 100
            --
            (omPat (omVar "Nil") [ (["Cons", "n", wcard], omVar "n") , (["Nil"], omLit (PInt 100)) ])
            (Right (Value (PInt 100)))

------------------------------------------------------------------------------------------------------

        testEval "match Cons(1, Cons(2, Nil)) with | Cons(_, xs) = match xs with | Cons(n, _) = n | Nil = 100 [2]"
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

------------------------------------------------------------------------------------------------------

evalRecordsTests :: SpecWith ()
evalRecordsTests = do

    describe "Eval records" $ do

        testEval "{ one = 1, two = 2 }.two [2]"
            --
            -- .two({ one = 1, two = 2 })
            --
            -- { one = 1, two = 2 }.two
            --
            (omApp [omVar ".two", example2])
            (Right (Value (PInt 2)))

        testEval "{ one = 1, two = 2 }.one [1]"
            --
            -- .one(two, { one = 1, two = 2 })
            --
            -- { one = 1, two = 2 }.one
            --
            (omApp [omVar ".one", example2])
            (Right (Value (PInt 1)))

    describe "Eval records (pattern matching)" $ do

        testEval "Example #1"
            --
            -- let 
            --   r =
            --     { one = 1, two = 2 }   
            --   in
            --     match r with
            --       | #(row) = 
            --           match row with
            --             | { one = o | _ } = o
            --
            (omLet "r"
                example2
                (omPat 
                    (omVar "r")
                    [(["#", "row"], 
                        omPat 
                            (omVar "row")
                            [(["{one}", "o", wcard], omVar "o")]
                    )]))
            (Right (Value (PInt 1)))

        testEval "Example #2"
            --
            -- let 
            --   r =
            --     { two = 2, one = 1 }   
            --   in
            --     match r with
            --       | #(row) = 
            --           match row with
            --             | { one = o | _ } = o
            --
            (omLet "r"
                example3
                (omPat 
                    (omVar "r")
                    [(["#", "row"], 
                        omPat 
                            (omVar "row")
                            [(["{one}", "o", wcard], omVar "o")]
                    )]))
            (Right (Value (PInt 1)))

        testEval "Example #3"
            --
            -- let 
            --   r =
            --     { one = 1, two = 2 }   
            --   in
            --     match r with
            --       | { one = o | two = t } = t
            --
            --               |||
            --                |
            --                v
            -- let 
            --   r =
            --     { one = 1, two = 2 }   
            --   in
            --     match r with
            --       | #(row) = 
            --           match row with
            --             | { one = o | r1 } = 
            --                 match r1 with
            --                   | { two = t } = t
            (omLet "r"
                example2
                (omPat 
                    (omVar "r")
                    [(["#", "row"], 
                        (omPat 
                            (omVar "row")
                            [(["{one}", "o", "r1"], 
                                (omPat 
                                    (omVar "r1")
                                    [(["{two}", "t", "{}"], omVar "t")]
                                )
                            )])
                    )]))
            (Right (Value (PInt 2)))

        testEval "Example #4"
            --
            -- let 
            --   r =
            --     { two = 2, one = 1 }   
            --   in
            --     match r with
            --       | #(row) = 
            --           match row with
            --             | { one = o | r1 } = 
            --                 match r1 with
            --                   | { two = t } = t
            (omLet "r"
                example3
                (omPat 
                    (omVar "r")
                    [(["#", "row"], 
                        (omPat 
                            (omVar "row")
                            [(["{one}", "o", "r1"], 
                                (omPat 
                                    (omVar "r1")
                                    [(["{two}", "t", "{}"], omVar "t")]
                                )
                            )])
                    )]))
            (Right (Value (PInt 2)))

        testEval "Example #5"
            --
            -- let 
            --   r =
            --     { one = 1, two = 2 }   
            --   in
            --     match r with
            --       | { one = o | r } = r
            --
            --               |||
            --                |
            --                v
            -- let 
            --   r =
            --     { one = 1, two = 2 }   
            --   in
            --     match r with
            --       | #(row) = 
            --           match row with
            --             | { one = o | r } = r
            (omLet "r"
                example2
                (omPat 
                    (omVar "r")
                    [(["#", "row"], 
                        (omPat 
                            (omVar "row")
                            [(["{one}", "o", "r"], omVar "r")])
                    )]))
            (Right (Data "{two}" [Value (PInt 2), Data "{}" []]))

        testEval "Example #6"
            --
            -- let 
            --   r =
            --     { one = 1 }   
            --   in
            --     match r with
            --       | { one = o | r } = r
            --
            --               |||
            --                |
            --                v
            -- let 
            --   r =
            --     { one = 1 }   
            --   in
            --     match r with
            --       | #(row) = 
            --           match row with
            --             | { one = o | r } = r
            (omLet "r"
                example4
                (omPat 
                    (omVar "r")
                    [(["#", "row"], 
                        (omPat 
                            (omVar "row")
                            [(["{one}", "o", "r"], omVar "r")])
                    )]))
            (Right (Data "{}" []))
