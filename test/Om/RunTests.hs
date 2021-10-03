{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Om.RunTests where

import Control.Monad.Except
import Data.Either.Extra (mapLeft)
import Data.Text (Text, unpack, pack)
import Om.Eval
import Om.Eval.Strict
import Om.Lang
import Om.Lang.Parser
import Om.Plug
import Om.Plug.Nats
import Om.Plug.Records
import Om.Prim
import Om.Prim.Basic
import Om.Prim.BasicNats
import Om.Prim.FunLang
import Test.Hspec
import Text.Megaparsec
import qualified Om.Plug.Constructors.Parser as Constructors
import qualified Om.Plug.Records.Parser as Records
import qualified Om.Prim.Basic as Basic
import qualified Om.Prim.Basic.Parser as Basic
import qualified Om.Prim.BasicNats as BasicNats
import qualified Om.Prim.BasicNats.Parser as BasicNats
import qualified Om.Prim.FunLang as FunLang
import qualified Om.Prim.FunLang.Parser as FunLang

runExprTests :: SpecWith ()
runExprTests = do

    describe "Run expression" $ do

        testRun runBasicExpr
            "let fact = n => if $eq(n, 0) then 1 else $mul(n, fact($sub(n, 1))) in fact(8)"
            (Right (Value (Basic.Int 40320)))

        testRun runBasicExpr
            "match Cons(1, Cons(2, Cons (3, Nil))) | Cons(_, xs) = match xs | Cons(n, _) = n end | Nil = 100"
            (Right (Value (Basic.Int 2)))

        testRun runBasicExpr
            "match Nil | Cons(_, xs) = match xs | Cons(n, _) = n end | Nil = 100"
            (Right (Value (Basic.Int 100)))

    describe "Run expression (nats)" $ do

        testRun runBasicNatsExpr
            "zero"
            (Right (Value (BasicNats.Nat 0)))

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

    describe "Run expression (records)" $ do

        testRun runBasicNatsExpr
            "let r = { a = 5 } in match r | #(row) = match row | { a = a } = a"
            (Right (Value (BasicNats.Int 5)))

        testRun runBasicNatsExpr
            "let r = { a = 5 } in match r | #(row) = match row | { a = _ | r } = r"
            (Right (Data "{}" []))

        testRun runBasicNatsExpr
            "let r = { a = 5, b = 4 } in match r | #(row) = match row | { a = _ | r } = r"
            (Right (Data "{b}" [Value (BasicNats.Int 4), Data "{}" []]))

        testRun runBasicNatsExpr
            "let r = { a = 5, b = 4 } in match r | #(row) = match row | { a = n | _ } = n"
            (Right (Value (BasicNats.Int 5)))

        testRun runBasicNatsExpr
            ".two({ one = 1, two = 2 })"
            (Right (Value (BasicNats.Int 2)))

parseAndRun
  :: (Show p, PrimType p Bool)
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

parseAndRunIO
  :: (Show p, PrimType p Bool)
  => PrimEnvT IO p
  -> ParserContext p
  -> Plugin p (EvalT IO p)
  -> Text
  -> IO (ResultT IO p)
parseAndRunIO primEnv context plugins input = runExpr >>= \case
    Left e ->
        error (show e)
    Right result -> do
        putStrLn ("(" <> toString result <> ")")
        pure result
  where
    runExpr = do
        expr <- either (error . show) pure (runParserStack exprParser input context)
        evalExprT expr primEnv plugins

testRun :: (Eq p) => (Text -> Either Text (Result p)) -> Text -> Either Text (Result p) -> SpecWith ()
testRun fun input expect =
    it (unpack input) (fun input == expect)

testRunIO :: (Eq p) => (Text -> IO (Either Text (ResultT IO p))) -> Text -> Either Text (ResultT IO p) -> SpecWith ()
testRunIO fun input expect = do
    result <- runIO (fun input)
    it (unpack input) (result == expect)

runBasicExpr :: Text -> Either Text (Result BasicPrim)
runBasicExpr = parseAndRun
    basicPrelude
    (Constructors.parser <> Basic.parser <> Records.parser)
    recordsPlugin

runBasicNatsExpr :: Text -> Either Text (Result BasicNatsPrim)
runBasicNatsExpr = parseAndRun
    basicNatsPrelude
    (Constructors.parser <> BasicNats.parser <> Records.parser)
    (natsPlugin <> recordsPlugin)

runFunExpr :: Text -> IO (ResultT IO FunPrim)
runFunExpr = parseAndRunIO
    funPrelude
    (Constructors.parser <> FunLang.parser <> Records.parser)
    recordsPlugin
