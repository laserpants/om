{-# LANGUAGE OverloadedStrings #-}
module Om.Lang.Parser where

import Control.Monad.Combinators.Expr
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import Data.Void
import Om.Lang
import Om.Util
import Text.Megaparsec hiding (token)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char as Megaparsec
import qualified Text.Megaparsec.Char.Lexer as Lexer

type Parser = Parsec Void Text

spaces :: Parser ()
spaces = Lexer.space
    space1
    (Lexer.skipLineComment "--")
    (Lexer.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme spaces

token :: Text -> Parser Text
token = Lexer.symbol spaces

parens :: Parser a -> Parser a
parens = between (token "(") (token ")")

commaSep :: Parser a -> Parser [a]
commaSep parser = parser `sepBy1` token ","

components :: Parser a -> Parser [a]
components = parens . commaSep

validChar :: Parser Char
validChar = alphaNumChar <|> char '_'

withInitial :: Parser Char -> Parser Text
withInitial parser = pack <$> ((:) <$> parser <*> many validChar)

keyword :: Text -> Parser ()
keyword tok = Megaparsec.string tok
    *> notFollowedBy alphaNumChar
    *> spaces

reserved :: [Text]
reserved =
    [ "else"
    , "if"
    , "let"
    , "match"
    , "then"
    , "with"
    ]

--    , "false"
--    , "succ"
--    , "true"
--    , "zero"

word :: Parser Text -> Parser Text
word parser = lexeme $ try $ do
    var <- parser
    if var `elem` reserved
        then fail ("Reserved word " <> unpack var)
        else pure var

nameParser :: Parser Text
nameParser = word (withInitial (lowerChar <|> char '_'))

wordParser :: Parser Text
wordParser = word (pack <$> many validChar)

exprParser :: Parser (Om p)
exprParser = (`makeExprParser` []) $
    try parseApp
        <|> parens exprParser
        <|> parser
  where
    parser = parseIf
        <|> parseLet
        <|> parsePat
        <|> try parseLam
        <|> parseVar
        <|> parsePrim

parseIf :: Parser (Om p)
parseIf = omIf
    <$> (keyword "if"   *> exprParser)
    <*> (keyword "then" *> exprParser)
    <*> (keyword "else" *> exprParser)

parseLet :: Parser (Om p)
parseLet = do
    keyword "let"
    name <- nameParser <* token "="
    expr <- exprParser <* keyword "in"
    body <- exprParser
    pure (omLet name expr body)

parseApp :: Parser (Om p)
parseApp = do
    fun <- funParser
    args <- components exprParser
    pure (omApp (fun:args))
  where
    funParser = try (parens exprParser)
        <|> try (omVar <$> (word (withInitial (char '$'))))
        <|> omVar <$> wordParser

parseLam :: Parser (Om p)
parseLam = do
    name <- nameParser
    token "=>"
    body <- exprParser
    pure (omLam name body)

parsePat :: Parser (Om p)
parsePat = do
    keyword "match"
    expr <- exprParser <* keyword "with"
    clauses <- some parseClause
    pure (omPat expr clauses)

parseClause :: Parser (Names, Om p)
parseClause = do
    token "|"
    names <- try (pure <$> wildcard) <|> do
        p <- wordParser
        ps <- optional args <#> fromMaybe []
        pure (p:ps)
    token "="
    expr <- exprParser
    pure (names, expr)
  where
    args = components (wildcard <|> wordParser)

wildcard :: Parser Name
wildcard = token "_" $> wcard

parseVar :: Parser (Om p)
parseVar = omVar <$> nameParser

parsePrim :: Parser (Om p)
parsePrim = char '$' *> (omPrim <$> nameParser)
