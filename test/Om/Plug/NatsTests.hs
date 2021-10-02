{-# LANGUAGE OverloadedStrings #-}
module Om.Plug.NatsTests where

import Data
import Data.Text (Text, unpack, pack)
import Om.Eval
import Om.Eval.Strict
import Om.Lang
import Om.Plug.Constructors
import Om.Plug.Nats
import Om.Plug.Records
import Om.Prim.Basic
import Om.Prim.BasicNats
import Test.Hspec
import qualified Om.Plug.Records.Parser as Records
import qualified Om.Prim.Basic as Basic
import qualified Om.Prim.Basic.Parser as Basic
import qualified Om.Prim.BasicNats as BasicNats
import qualified Om.Prim.BasicNats.Parser as BasicNats

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

testEvalBasicNats :: Text -> Om BasicNatsPrim -> Either Error (Result BasicNatsPrim) -> SpecWith ()
testEvalBasicNats dscr om expect =
    it (unpack dscr) (expect == result)
  where
    result = evalExpr om basicNatsPrelude (natsPlugin <> constructorsPlugin <> recordsPlugin)
