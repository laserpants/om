{-# LANGUAGE OverloadedStrings #-}
module Om.Lang.ParserTests
  ( parserTests
  ) where

import Data
import Data.Either (isLeft)
import Data.Text (Text, unpack, pack)
import Om.Lang
import Om.Lang.Parser
import Om.Prim.Basic
import Om.Prim.BasicNats
import Test.Hspec
import qualified Om.Plug.Records.Parser as Records
import qualified Om.Plug.Constructors.Parser as Constructors
import qualified Om.Prim.Basic as Basic
import qualified Om.Prim.Basic.Parser as Basic
import qualified Om.Prim.BasicNats as BasicNats
import qualified Om.Prim.BasicNats.Parser as BasicNats

parserTests :: SpecWith ()
parserTests = do

    describe "Parser" $ do
        testParse exprParser exampleContext1
            "let x = y in z"
            (omLet "x" (omVar "y") (omVar "z") :: Om BasicPrim)

        testParse exprParser exampleContext1
            "Cons(x, xs)"
            (omData "Cons" [omVar "x", omVar "xs"] :: Om BasicPrim)

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
            (omPat (omData "Cons" [omVar "x", omVar "y"]) [(["Cons", "x", "$_"], omVar "x"), (["Nil"], omVar "y")] :: Om BasicPrim)

        testParse exprParser exampleContext1
            "match Nil | Cons(x, _) = x | Nil = y"
            (omPat (omData "Nil" []) [(["Cons", "x", "$_"], omVar "x"), (["Nil"], omVar "y")] :: Om BasicPrim)

        testParse exprParser exampleContext1
            "let x = y => y in x(y)"
            (omLet "x" (omLam "y" (omVar "y")) (omApp [omVar "x", omVar "y"]) :: Om BasicPrim)

        testParse exprParser exampleContext1
            "let x = (y => y) in x(y)"
            (omLet "x" (omLam "y" (omVar "y")) (omApp [omVar "x", omVar "y"]) :: Om BasicPrim)

        testParse exprParser exampleContext1
            "Nil"
            (omData "Nil" [] :: Om BasicPrim)

        testParse exprParser exampleContext1
            "(Nil)"
            (omData "Nil" [] :: Om BasicPrim)

        testParse exprParser exampleContext1
            "1"
            (omLit (Basic.Int 1) :: Om BasicPrim)

        testParse exprParser exampleContext1
            "(1)"
            (omLit (Basic.Int 1) :: Om BasicPrim)

        testParse exprParser exampleContext1
            "Cons(1, Cons(2, Cons (3, Nil)))"
            (omData "Cons" [omLit (Basic.Int 1), omData "Cons" [omLit (Basic.Int 2), omData "Cons" [omLit (Basic.Int 3), omData "Nil" []]]])

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

        testParse exprParser exampleContext1
            "match Cons(1, Cons(2, Cons (3, Nil))) | Cons(_, xs) = match xs | Cons(n, _) = n end | Nil = 100"
            (omPat
                (omData "Cons"
                    [ omLit (Basic.Int 1)
                    , omData "Cons"
                        [ omLit (Basic.Int 2)
                        , omData "Cons"
                            [ omLit (Basic.Int 3)
                            , omData "Nil" []]]])
                                [ (["Cons", wcard, "xs"],
                                    omPat (omVar "xs")
                                        [ (["Cons", "n", wcard], omVar "n")
                                        ])
                                , (["Nil"], omLit (Basic.Int 100))
                                ])

    describe "Parser (nats)" $ do
        testParse exprParser exampleContext2
            "let m = succ(succ(zero)) in let n = succ(succ(succ(zero))) in $add(m, n)"
            (omLet "m"
                (omData "succ"
                    [ omData "succ"
                        [ omData "zero" []
                        ]
                    ])
                (omLet "n"
                    (omData "succ"
                        [ omData "succ"
                            [ omData "succ"
                                [ omData "zero" [] ]
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
        testParse exprParser (Basic.parser <> Records.parser)
            "{ foo = 1, baz = 2 }"
            (omData "#" [omData "{foo}" [omLit (Basic.Int 1), omData "{baz}" [omLit (Basic.Int 2), omVar "{}"]]])

        testParse exprParser (Basic.parser <> Records.parser)
            "let r = { a = 5 } in match r | { a = a } = a"
            (omLet "r"
                (omData "#" [omData "{a}" [omLit (Basic.Int 5), omVar "{}"]])
                (omPat (omVar "r")
                    [ (["{a}", "a", "{}"], omVar "a")
                    ]))

        testParse exprParser (Basic.parser <> Records.parser)
            ".two(x)"
            (omApp [omVar ".two", omVar "x"])

        testParse exprParser (Basic.parser <> Records.parser)
            ".two({ one = 1, two = 2 })"
            (omApp [omVar ".two", omData "#" [omData "{one}" [omLit (Basic.Int 1), omData "{two}" [omLit (Basic.Int 2), omVar "{}"]]]])

    describe "Parser (pattern matching of records)" $ do
        testParse exprParser (Basic.parser <> Records.parser)
            "match xs | { foo = a } = a end"
            (omPat (omVar "xs")
                [ (["{foo}", "a", "{}"], omVar "a")
                ])

        testParse exprParser (Basic.parser <> Records.parser)
            "match xs | { foo = _ | r } = r end"
            (omPat (omVar "xs")
                [ (["{foo}", "$_", "r"], omVar "r")
                ])

    describe "Parser (failures)" $ do
        testParseFail exprParser BasicNats.parser
            "let true = foo in moo"

        testParseFail exprParser BasicNats.parser
            "let zero = foo in moo"

testParse :: (Eq a) => Parser p a -> ParserContext p -> Text -> a -> SpecWith ()
testParse parser context input expect =
    it (unpack input) (runParserStack parser input context == Right expect)

testParseFail :: (Eq a) => Parser p a -> ParserContext p -> Text -> SpecWith ()
testParseFail parser context input =
    it (unpack input) (isLeft (runParserStack parser input context))
