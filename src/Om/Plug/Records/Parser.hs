{-# LANGUAGE OverloadedStrings #-}
module Om.Plug.Records.Parser 
  ( parserContext
  ) where

import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Om.Lang
import Om.Lang.Parser
import Om.Prim.BasicNats
import Om.Util
import Text.Megaparsec hiding (token)
import qualified Om.Prim.Basic.Parser as Basic
import qualified Text.Megaparsec.Char.Lexer as Lexer

braces :: Parser p a -> Parser p a
braces = between (token "{") (token "}")

fields :: Parser p a -> Parser p [(Name, a)]
fields parser = commaSep $ (,)
    <$> nameParser
    <*> (token "=" *> parser)

parseRecord :: Parser p (Om p)
parseRecord = braces $ do
    pairs <- fields exprParser
    pure (omData "#" [foldr fun (omVar "{}") pairs])
  where
    fun (name, expr) next =
        omData ("{" <> name <> "}") [expr, next]

parseRecordPattern :: Parser p [Name]
parseRecordPattern = braces $ do
    key <- nameParser
    token "="
    pat <- wildcard <|> nameParser
    nxt <- optional (token "|" *> (wildcard <|> nameParser))
    pure ["{" <> key <> "}", pat, fromMaybe "{}" nxt]

parserContext :: ParserContext p
parserContext = mempty
    { contextConstructors = ["#"]
    , contextExprParser = parseRecord
    , contextPatternParser = parseRecordPattern
    }
