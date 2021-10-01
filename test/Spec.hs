{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec

import Control.Monad.Except
import Control.Monad.Reader
import Data.Either (isLeft)
import Data.Either.Extra (mapLeft)
import Data.Text (Text, unpack, pack)
import Debug.Trace
import Om.Eval
import Om.Eval.Strict
import Om.Lang
import Om.Lang.Parser
import Om.Plug
import Om.Plug.Constructor
import Om.Plug.Nats
import Om.Plug.Records
import Om.Prim
import Om.Prim.Basic
import Om.Prim.BasicNats
import Om.Util
import Text.Megaparsec
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Om.Plug.Records.Parser as Records
import qualified Om.Prim.Basic as Basic
import qualified Om.Prim.Basic.Parser as Basic
import qualified Om.Prim.BasicNats as BasicNats
import qualified Om.Prim.BasicNats.Parser as BasicNats

main :: IO ()
main = hspec $ do
    evalTests
    evalRecordsTests
    evalNatsTests
    parserTests
    runTests

------------------------------------------------------------------------------------------------------

parseAndRun
  :: (PrimType p Bool)
  => PrimEnv p
  -> ParserContext p
  -> Plugin p (Eval p)
  -> Text
  -> Either Text (Result p)
parseAndRun primEnv context plugins input = do
    om <- mapLeft (\e -> traceShow e $ "Parser error") parse
    mapLeft (pack . show) (eval om)
  where
    parse = runParserStack exprParser input context
    eval expr = evalExpr expr primEnv plugins

------------------------------------------------------------------------------------------------------

testEvalBasic :: Text -> Om BasicPrim -> Either Error (Result BasicPrim) -> SpecWith ()
testEvalBasic dscr om expect =
    it (unpack dscr) (expect == result)
  where
    result = evalExpr om basicPrelude (constructorPlugin <> recordsPlugin)

testEvalBasicNats :: Text -> Om BasicNatsPrim -> Either Error (Result BasicNatsPrim) -> SpecWith ()
testEvalBasicNats dscr om expect =
    it (unpack dscr) (expect == result)
  where
    result = evalExpr om basicNatsPrelude (natsPlugin <> constructorPlugin <> recordsPlugin)

------------------------------------------------------------------------------------------------------

-- Cons(1, Cons(2, (Cons 3, Nil)))
example1 :: Om BasicPrim
example1 =
    omData "Cons"
        [ omLit (Basic.Int 1)
        , omData "Cons"
            [ omLit (Basic.Int 2)
            , omData "Cons"
                [ omLit (Basic.Int 3)
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
            [ omLit (Basic.Int 1)
            , omData "{two}"
                [ omLit (Basic.Int 2)
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
            [ omLit (Basic.Int 2)
            , omData "{one}"
                [ omLit (Basic.Int 1)
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
            [ omLit (Basic.Int 1)
            , omData "{}" []
            ]
        ]

exampleContext1 :: ParserContext BasicPrim
exampleContext1 = Basic.parserContext <> Records.parserContext

exampleContext2 :: ParserContext BasicNatsPrim
exampleContext2 = BasicNats.parserContext <> Records.parserContext

------------------------------------------------------------------------------------------------------

evalTests :: SpecWith ()
evalTests =

    describe "Eval" $ do

        testEvalBasic "8 factorial [40320]"
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

------------------------------------------------------------------------------------------------------

        testEvalBasic "Cons(1, Cons(2, (Cons 3, Nil)))"
            example1
            (Right (Data "Cons" [Value (Basic.Int 1), Data "Cons" [Value (Basic.Int 2), Data "Cons" [Value (Basic.Int 3), Data "Nil" []]]]))

------------------------------------------------------------------------------------------------------

        testEvalBasic "Cons(1, Cons(2, (Cons 3, Nil)))"
            (omData "Cons" [omLit (Basic.Int 1), omData "Cons" [omLit (Basic.Int 2), omData "Cons" [omLit (Basic.Int 3), omVar "Nil"]]])
            (Right (Data "Cons" [Value (Basic.Int 1), Data "Cons" [Value (Basic.Int 2), Data "Cons" [Value (Basic.Int 3), Data "Nil" []]]]))

------------------------------------------------------------------------------------------------------

        testEvalBasic "match Cons(1, Cons(2, Cons (3, Nil))) | Cons(n, _) = n [1]"
            --
            -- match Cons(1, Cons(2, (Cons 3, Nil)))
            --   | Cons(n, _) = n
            --
            (omPat example1 [ (["Cons", "n", wcard], omVar "n") ])
            (Right (Value (Basic.Int 1)))

------------------------------------------------------------------------------------------------------

        testEvalBasic "match Nil | Cons(n, _) = n | Nil = 100 [100]"
            --
            -- match Nil
            --   | Cons(n, _) = n
            --   | Nil = 100
            --
            (omPat (omVar "Nil") [ (["Cons", "n", wcard], omVar "n") , (["Nil"], omLit (Basic.Int 100)) ])
            (Right (Value (Basic.Int 100)))

------------------------------------------------------------------------------------------------------

        testEvalBasic "match Cons(1, Cons(2, Cons (3, Nil))) | Cons(_, xs) = match xs | Cons(n, _) = n end | Nil = 100 [2]"
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
                    (omPat (omVar "xs")
                        [ (["Cons", "n", wcard], omVar "n") ]))
                , (["Nil"], omLit (Basic.Int 100))
                ])
            (Right (Value (Basic.Int 2)))

------------------------------------------------------------------------------------------------------

evalRecordsTests :: SpecWith ()
evalRecordsTests = do

    describe "Eval records" $ do

        testEvalBasic "{ one = 1, two = 2 }.two [2]"
            --
            -- .two({ one = 1, two = 2 })
            --
            -- { one = 1, two = 2 }.two
            --
            (omApp [omVar ".two", example2])
            (Right (Value (Basic.Int 2)))

        testEvalBasic "{ one = 1, two = 2 }.one [1]"
            --
            -- .one(two, { one = 1, two = 2 })
            --
            -- { one = 1, two = 2 }.one
            --
            (omApp [omVar ".one", example2])
            (Right (Value (Basic.Int 1)))

    describe "Eval records (pattern matching)" $ do

        testEvalBasic "Example #1"
            --
            -- let
            --   r =
            --     { one = 1, two = 2 }
            --   in
            --     match r
            --       | #(row) =
            --           match row
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
            (Right (Value (Basic.Int 1)))

        testEvalBasic "Example #2"
            --
            -- let
            --   r =
            --     { two = 2, one = 1 }
            --   in
            --     match r
            --       | #(row) =
            --           match row
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
            (Right (Value (Basic.Int 1)))

        testEvalBasic "Example #3"
            --
            -- let
            --   r =
            --     { one = 1, two = 2 }
            --   in
            --     match r
            --       | { one = o | two = t } = t
            --
            --               |||
            --                |
            --                v
            -- let
            --   r =
            --     { one = 1, two = 2 }
            --   in
            --     match r
            --       | #(row) =
            --           match row
            --             | { one = o | r1 } =
            --                 match r1
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
            (Right (Value (Basic.Int 2)))

        testEvalBasic "Example #4"
            --
            -- let
            --   r =
            --     { two = 2, one = 1 }
            --   in
            --     match r
            --       | #(row) =
            --           match row
            --             | { one = o | r1 } =
            --                 match r1
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
            (Right (Value (Basic.Int 2)))

        testEvalBasic "Example #5"
            --
            -- let
            --   r =
            --     { one = 1, two = 2 }
            --   in
            --     match r
            --       | { one = o | r } = r
            --
            --               |||
            --                |
            --                v
            -- let
            --   r =
            --     { one = 1, two = 2 }
            --   in
            --     match r
            --       | #(row) =
            --           match row
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
            (Right (Data "{two}" [Value (Basic.Int 2), Data "{}" []]))

        testEvalBasic "Example #6"
            --
            -- let
            --   r =
            --     { one = 1 }
            --   in
            --     match r
            --       | { one = o | r } = r
            --
            --               |||
            --                |
            --                v
            -- let
            --   r =
            --     { one = 1 }
            --   in
            --     match r
            --       | #(row) =
            --           match row
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

------------------------------------------------------------------------------------------------------

evalNatsTests :: SpecWith ()
evalNatsTests = do

    describe "Eval nats" $ do

        testEvalBasicNats "succ(succ(zero))"
            (omApp
                [ omVar "succ"
                , omApp
                    [ omVar "succ"
                    , omVar "zero"
                    ]
                ])
            (Right (Value (BasicNats.Nat 2)))

        testEvalBasicNats "zero"
            (omVar "zero")
            (Right (Value (BasicNats.Nat 0)))

        testEvalBasicNats "let m = succ(succ(zero)) in let n = succ(succ(succ(zero))) in $add(m, n) [Nat 5]"
            (omLet "m"
                (omApp
                    [ omVar "succ"
                    , omApp
                        [ omVar "succ"
                        , omVar "zero"
                        ]
                    ])
                (omLet "n"
                    (omApp
                        [ omVar "succ"
                        , omApp
                            [ omVar "succ"
                            , omApp
                                [ omVar "succ"
                                , omVar "zero"
                                ]
                            ]
                        ])
                    (omApp
                        [ omPrim "add"
                        , omVar "m"
                        , omVar "n"
                        ])))
            (Right (Value (BasicNats.Nat 5)))

    describe "Eval nats (pattern matching)" $ do

        testEvalBasicNats "match succ(succ(succ(zero))) | succ(n) = n [2]"
            (omPat
                (omApp
                    [ omVar "succ"
                    , omApp
                        [ omVar "succ"
                        , omApp
                            [ omVar "succ"
                            , omVar "zero"
                            ]
                        ]
                    ])
                [(["succ", "n"], omVar "n")])
            (Right (Value (BasicNats.Nat 2)))

        testEvalBasicNats "match succ(succ(succ(zero))) | zero = 1 | succ(n) = n [2]"
            (omPat
                (omApp
                    [ omVar "succ"
                    , omApp
                        [ omVar "succ"
                        , omApp
                            [ omVar "succ"
                            , omVar "zero"
                            ]
                        ]
                    ])
                [ (["zero"], omLit (BasicNats.Nat 1))
                , (["succ", "n"], omVar "n")
                ])
            (Right (Value (BasicNats.Nat 2)))

        testEvalBasicNats "match zero | zero = 1 | succ(n) = n [1]"
            (omPat
                (omVar "zero")
                [ (["zero"], omLit (BasicNats.Nat 1))
                , (["succ", "n"], omVar "n")
                ])
            (Right (Value (BasicNats.Nat 1)))

        testEvalBasicNats "match zero | succ(n) = n | zero = 1 [1]"
            (omPat
                (omVar "zero")
                [ (["succ", "n"], omVar "n")
                , (["zero"], omLit (BasicNats.Nat 1))
                ])
            (Right (Value (BasicNats.Nat 1)))

        testEvalBasicNats "match succ(succ(succ(zero))) | succ(n) = n | zero = 1 [2]"
            (omPat
                (omApp
                    [ omVar "succ"
                    , omApp
                        [ omVar "succ"
                        , omApp
                            [ omVar "succ"
                            , omVar "zero"
                            ]
                        ]
                    ])
                [ (["succ", "n"], omVar "n")
                , (["zero"], omLit (BasicNats.Nat 1))
                ])
            (Right (Value (BasicNats.Nat 2)))

------------------------------------------------------------------------------------------------------

testParse :: (Eq a) => Parser p a -> ParserContext p -> Text -> a -> SpecWith ()
testParse parser context input expect =
    it (unpack input) (runParserStack parser input context == Right expect)

testParseFail :: (Eq a) => Parser p a -> ParserContext p -> Text -> SpecWith ()
testParseFail parser context input =
    it (unpack input) (isLeft (runParserStack parser input context))

------------------------------------------------------------------------------------------------------

parserTests :: SpecWith ()
parserTests = do

    describe "Parser" $ do
        testParse exprParser exampleContext1
            "let x = y in z"
            (omLet "x" (omVar "y") (omVar "z") :: Om BasicPrim)

        testParse exprParser exampleContext1
            "Cons(x, xs)"
            (omApp [omVar "Cons", omVar "x", omVar "xs"] :: Om BasicPrim)

        testParse exprParser exampleContext1
            "fun(x)"
            (omApp [omVar "fun", omVar "x"] :: Om BasicPrim)

        testParse exprParser exampleContext1
            "$fun(x)"
            (omApp [omVar "$fun", omVar "x"] :: Om BasicPrim)

        testParse exprParser exampleContext1
            "$fun"
            (omVar "$fun" :: Om BasicPrim)

        testParse exprParser exampleContext1
            "($fun)"
            (omVar "$fun" :: Om BasicPrim)

        testParse exprParser exampleContext1
            "if f(x, y) then z else z => z"
            (omIf (omApp [omVar "f", omVar "x", omVar "y"]) (omVar "z") (omLam "z" (omVar "z")) :: Om BasicPrim)

        testParse exprParser exampleContext1
            "(x => x)(y)"
            (omApp [omLam "x" (omVar "x"), omVar "y"] :: Om BasicPrim)

        testParse exprParser exampleContext1
            "((x => x)(y))"
            (omApp [omLam "x" (omVar "x"), omVar "y"] :: Om BasicPrim)

        testParse exprParser exampleContext1
            "(((x => x))(y))"
            (omApp [omLam "x" (omVar "x"), omVar "y"] :: Om BasicPrim)

        testParse exprParser exampleContext1
            "($fun(x))"
            (omApp [omVar "$fun", omVar "x"] :: Om BasicPrim)

        testParse exprParser exampleContext1
            "(($fun(x)))"
            (omApp [omVar "$fun", omVar "x"] :: Om BasicPrim)

        testParse exprParser exampleContext1
            "x"
            (omVar "x" :: Om BasicPrim)

        testParse exprParser exampleContext1
            "(x)"
            (omVar "x" :: Om BasicPrim)

        testParse exprParser exampleContext1
            "((x))"
            (omVar "x" :: Om BasicPrim)

        testParse exprParser exampleContext1
            "match xs | _ = x"
            (omPat (omVar "xs") [(["$_"], omVar "x")] :: Om BasicPrim)

        testParse exprParser exampleContext1
            "match xs | Cons(x, xs) = x | Nil = y"
            (omPat (omVar "xs") [(["Cons", "x", "xs"], omVar "x"), (["Nil"], omVar "y")] :: Om BasicPrim)

        testParse exprParser exampleContext1
            "match xs | Cons(x, xs) = x | Nil = y end"
            (omPat (omVar "xs") [(["Cons", "x", "xs"], omVar "x"), (["Nil"], omVar "y")] :: Om BasicPrim)

        testParse exprParser exampleContext1
            "match xs | Cons(x, _) = x | Nil = y"
            (omPat (omVar "xs") [(["Cons", "x", "$_"], omVar "x"), (["Nil"], omVar "y")] :: Om BasicPrim)

        testParse exprParser exampleContext1
            "match Cons(x, y) | Cons(x, _) = x | Nil = y"
            (omPat (omApp [omVar "Cons", omVar "x", omVar "y"]) [(["Cons", "x", "$_"], omVar "x"), (["Nil"], omVar "y")] :: Om BasicPrim)

        testParse exprParser exampleContext1
            "match Nil | Cons(x, _) = x | Nil = y"
            (omPat (omVar "Nil") [(["Cons", "x", "$_"], omVar "x"), (["Nil"], omVar "y")] :: Om BasicPrim)

        testParse exprParser exampleContext1
            "let x = y => y in x(y)"
            (omLet "x" (omLam "y" (omVar "y")) (omApp [omVar "x", omVar "y"]) :: Om BasicPrim)

        testParse exprParser exampleContext1
            "let x = (y => y) in x(y)"
            (omLet "x" (omLam "y" (omVar "y")) (omApp [omVar "x", omVar "y"]) :: Om BasicPrim)

        testParse exprParser exampleContext1
            "Nil"
            (omVar "Nil" :: Om BasicPrim)

        testParse exprParser exampleContext1
            "(Nil)"
            (omVar "Nil" :: Om BasicPrim)

        testParse exprParser exampleContext1
            "1"
            (omLit (Basic.Int 1) :: Om BasicPrim)

        testParse exprParser exampleContext1
            "(1)"
            (omLit (Basic.Int 1) :: Om BasicPrim)

        testParse exprParser exampleContext1
            "Cons(1, Cons(2, Cons (3, Nil)))"
            (omData "Cons" [omLit (Basic.Int 1), omData "Cons" [omLit (Basic.Int 2), omData "Cons" [omLit (Basic.Int 3), omVar "Nil"]]])

        testParse exprParser exampleContext1
            "let fact = n => if $eq(n, 0) then 1 else $mul(n, fact($sub(n, 1))) in fact(8)"
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

        testParse exprParser Basic.parserContext
            "match Cons(1, Cons(2, Cons (3, Nil))) | Cons(_, xs) = match xs | Cons(n, _) = n end | Nil = 100"
            (omPat
                (omData "Cons"
                    [ omLit (Basic.Int 1)
                    , omData "Cons"
                        [ omLit (Basic.Int 2)
                        , omData "Cons"
                            [ omLit (Basic.Int 3)
                            , omVar "Nil"]]])
                                [ (["Cons", wcard, "xs"],
                                    (omPat (omVar "xs")
                                        [ (["Cons", "n", wcard], omVar "n")
                                        ]))
                                , (["Nil"], omLit (Basic.Int 100))
                                ])

    describe "Parser (nats)" $ do
        testParse exprParser exampleContext2
            "let m = succ(succ(zero)) in let n = succ(succ(succ(zero))) in $add(m, n)"
            (omLet "m"
                (omApp
                    [ omVar "succ"
                    , omApp
                        [ omVar "succ"
                        , omVar "zero"
                        ]
                    ])
                (omLet "n"
                    (omApp
                        [ omVar "succ"
                        , omApp
                            [ omVar "succ"
                            , omApp
                                [ omVar "succ"
                                , omVar "zero"
                                ]
                            ]
                        ])
                    (omApp
                        [ omPrim "add"
                        , omVar "m"
                        , omVar "n"
                        ])))

        testParse exprParser exampleContext2
            "match n | succ(n) = n end"
            (omPat (omVar "n")
                [ (["succ", "n"], omVar "n")
                ])

    describe "Parser (records)" $ do
        testParse exprParser (Basic.parserContext <> Records.parserContext)
            "{ foo = 1, baz = 2 }"
            (omData "#" [omData "{foo}" [omLit (Basic.Int 1), omData "{baz}" [omLit (Basic.Int 2), omVar "{}"]]])

    describe "Parser (pattern matching of records)" $ do
        testParse exprParser (Basic.parserContext <> Records.parserContext)
            "match xs | { foo = a } = a end"
            (omPat (omVar "xs")
                [ (["{foo}", "a", "{}"], omVar "a")
                ])

        testParse exprParser (Basic.parserContext <> Records.parserContext)
            "match xs | { foo = _ | r } = r end"
            (omPat (omVar "xs")
                [ (["{foo}", "$_", "r"], omVar "r")
                ])

    describe "Parser (failures)" $ do
        testParseFail exprParser BasicNats.parserContext
            "let true = foo in moo"

        testParseFail exprParser BasicNats.parserContext
            "let zero = foo in moo"

------------------------------------------------------------------------------------------------------

testRun :: (Eq p) => (Text -> Either Text (Result p)) -> Text -> Either Text (Result p) -> SpecWith ()
testRun fun input expect =
    it (unpack input) (fun input == expect)

------------------------------------------------------------------------------------------------------

runBasicExpr :: Text -> Either Text (Result BasicPrim)
runBasicExpr = parseAndRun
    basicPrelude
    (Basic.parserContext <> Records.parserContext)
    (constructorPlugin <> recordsPlugin)

runBasicNatsExpr :: Text -> Either Text (Result BasicNatsPrim)
runBasicNatsExpr = parseAndRun
    basicNatsPrelude
    (BasicNats.parserContext <> Records.parserContext)
    (constructorPlugin <> recordsPlugin <> natsPlugin)

runTests :: SpecWith ()
runTests = do

    describe "Run expression (nats)" $ do
        testRun runBasicNatsExpr
            "true"
            (Right (Value (BasicNats.Bool True)))

        testRun runBasicExpr
            "let fact = n => if $eq(n, 0) then 1 else $mul(n, fact($sub(n, 1))) in fact(8)"
            (Right (Value (Basic.Int 40320)))

        testRun runBasicNatsExpr
            "let m = succ(succ(zero)) in let n = succ(succ(succ(zero))) in $add(m, n)"
            (Right (Value (BasicNats.Nat 5)))

        testRun runBasicNatsExpr
            "(n => match n | succ(m) = m)(succ(succ(zero)))"
            (Right (Value (BasicNats.Nat 1)))

-- TODO
--        testRun runBasicNatsExpr
--            "(n => match n | succ(m) = m)(pack(5))"
--            (Right (Value (BasicNats.Nat 4)))
--
--        testRun runBasicNatsExpr
--            "unpack(succ(succ(succ(zero))))"
--            (Right (Value (BasicNats.Nat 3)))
