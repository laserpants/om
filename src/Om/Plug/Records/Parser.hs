{-# LANGUAGE OverloadedStrings #-}
module Om.Plug.Records.Parser
  ( parser
  ) where

import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Om.Lang
import Om.Lang.Parser
import Om.Prim.BasicNats
import Om.Util
import Text.Megaparsec hiding (token)
import Text.Megaparsec.Char
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

parseRecordConPattern :: Parser p [Name]
parseRecordConPattern = do
    char '#'
    pat <- parens (wildcard <|> nameParser)
    pure ["#", pat]

parseAccessor :: Parser p (Om p)
parseAccessor = do
    char '.'
    name <- nameParser
    expr <- parens exprParser
    pure (omApp [omVar ("." <> name), expr])

parser :: ParserContext p
parser = mempty
    { contextExprParser = parseAccessor <|> parseRecord
    , contextPatternParser = parseRecordConPattern <|> parseRecordPattern
    }
