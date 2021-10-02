{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Om.RunTests where

import Data.Either.Extra (mapLeft)
import Data.Text (Text, unpack, pack)
import Om.Eval
import Om.Eval.Strict
import Om.Lang
import Om.Lang.Parser
import Om.Plug
import Om.Plug.Constructors
import Om.Plug.Nats
import Om.Plug.Records
import Om.Prim
import Om.Prim.Basic
import Om.Prim.BasicNats
import Test.Hspec
import qualified Om.Plug.Records.Parser as Records
import qualified Om.Prim.Basic as Basic
import qualified Om.Prim.Basic.Parser as Basic
import qualified Om.Prim.BasicNats as BasicNats
import qualified Om.Prim.BasicNats.Parser as BasicNats

runExprTests :: SpecWith ()
runExprTests = do

    describe "Run expression" $ do

        testRun runBasicExpr
            "let fact = n => if $eq(n, 0) then 1 else $mul(n, fact($sub(n, 1))) in fact(8) [40320]"
            (Right (Value (Basic.Int 40320)))

        testRun runBasicExpr
            "match Cons(1, Cons(2, Cons (3, Nil))) | Cons(_, xs) = match xs | Cons(n, _) = n end | Nil = 100 [2]"
            (Right (Value (Basic.Int 2)))

        testRun runBasicExpr
            "match Nil | Cons(_, xs) = match xs | Cons(n, _) = n end | Nil = 100 [100]"
            (Right (Value (Basic.Int 100)))

    describe "Run expression (nats)" $ do

        testRun runBasicNatsExpr
            "true"
            (Right (Value (BasicNats.Bool True)))

        testRun runBasicNatsExpr
            "let m = succ(succ(zero)) in let n = succ(succ(succ(zero))) in $add(m, n)"
            (Right (Value (BasicNats.Nat 5)))

        testRun runBasicNatsExpr
            "(n => match n | succ(m) = m)(succ(succ(zero)))"
            (Right (Value (BasicNats.Nat 1)))

        testRun runBasicNatsExpr
            "(n => match n | succ(m) = m)($pack(5))"
            (Right (Value (BasicNats.Nat 4)))

        testRun runBasicNatsExpr
            "$unpack(succ(succ(succ(zero))))"
            (Right (Value (BasicNats.Int 3)))

        testRun runBasicNatsExpr
            "let m = succ(succ(zero)) in let n = $pack(3) in $add(m, n)"
            (Right (Value (BasicNats.Nat 5)))

parseAndRun
  :: (PrimType p Bool)
  => PrimEnv p
  -> ParserContext p
  -> Plugin p (Eval p)
  -> Text
  -> Either Text (Result p)
parseAndRun primEnv context plugins input = do
    om <- mapLeft (const "Parser error") parse
    mapLeft (pack . show) (eval om)
  where
    parse = runParserStack exprParser input context
    eval expr = evalExpr expr primEnv plugins

testRun :: (Eq p) => (Text -> Either Text (Result p)) -> Text -> Either Text (Result p) -> SpecWith ()
testRun fun input expect =
    it (unpack input) (fun input == expect)

runBasicExpr :: Text -> Either Text (Result BasicPrim)
runBasicExpr = parseAndRun
    basicPrelude
    (Basic.parserContext <> Records.parserContext)
    (constructorsPlugin <> recordsPlugin)

runBasicNatsExpr :: Text -> Either Text (Result BasicNatsPrim)
runBasicNatsExpr = parseAndRun
    basicNatsPrelude
    (BasicNats.parserContext <> Records.parserContext)
    (constructorsPlugin <> recordsPlugin <> natsPlugin)
