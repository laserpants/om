module Main where

import Om.Lang.Parser
import Text.Megaparsec

foo = runParser exprParser "" "let x = y in z"

zoo = runParser exprParser "" "Cons(x, xs)"

goo = runParser exprParser "" "fun(x)"

boo = runParser exprParser "" "$fun(x)"

moo = runParser exprParser "" "$fun"

main :: IO ()
main = pure ()
