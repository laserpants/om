{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Om.Lang.Parser
  ( ParserContext(..)
  , Parser
  , commaSep
  , components
  , exprParser
  , keyword
  , lexeme
  , nameParser
  , parens
  , runParserStack
  , token
  , wildcard
  , wordParser
  ) where

import Control.Monad.Combinators.Expr
import Control.Monad.Reader
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

data ParserContext p = ParserContext
  { contextReserved      :: [Text]
  , contextPrimParser    :: Parser p p
  , contextExprParser    :: Parser p (Om p)
  , contextPatternParser :: Parser p [Name]
  }

type Parser p = ParsecT Void Text (Reader (ParserContext p))

instance Semigroup (ParserContext p) where
    ParserContext r1 pp1 ep1 ptp1 <> ParserContext r2 pp2 ep2 ptp2 =
        ParserContext (r1 <> r2) (pp1 <|> pp2) (ep1 <|> ep2) (ptp1 <|> ptp2)

notImplemented :: Parser p q
notImplemented = fail "Not implemented"

instance Monoid (ParserContext p) where
    mempty = ParserContext [] notImplemented notImplemented notImplemented

runParserStack :: Parser p a -> Text -> ParserContext p -> Either (ParseErrorBundle Text Void) a
runParserStack parser = runReader . runParserT parser ""

spaces :: Parser p ()
spaces = Lexer.space
    space1
    (Lexer.skipLineComment "--")
    (Lexer.skipBlockComment "{-" "-}")

lexeme :: Parser p a -> Parser p a
lexeme = Lexer.lexeme spaces

token :: Text -> Parser p Text
token = Lexer.symbol spaces

parens :: Parser p a -> Parser p a
parens = between (token "(") (token ")")

commaSep :: Parser p a -> Parser p [a]
commaSep parser = parser `sepBy1` token ","

components :: Parser p a -> Parser p [a]
components = parens . commaSep

validChar :: Parser p Char
validChar = alphaNumChar <|> char '_'

withInitial :: Parser p Char -> Parser p Text
withInitial parser = pack <$> ((:) <$> parser <*> many validChar)

keyword :: Text -> Parser p ()
keyword tok = Megaparsec.string tok
    *> notFollowedBy alphaNumChar
    *> spaces

reserved :: [Text]
reserved =
    [ "else"
    , "end"
    , "if"
    , "let"
    , "match"
    , "then"
    ]

word :: Parser p Text -> Parser p Text
word parser = lexeme $ try $ do
    var <- parser
    ParserContext{ contextReserved } <- ask
    if var `elem` (reserved <> contextReserved)
        then fail ("Reserved word " <> unpack var)
        else pure var

nameParser :: Parser p Text
nameParser = word (withInitial (lowerChar <|> char '_'))

wordParser :: Parser p Text
wordParser = word (pack <$> some validChar)

exprParser :: Parser p (Om p)
exprParser = (`makeExprParser` []) $
    (ask >>= contextExprParser)
        <|> try parseApp
        <|> parens exprParser
        <|> parser
  where
    parser = parseIf
        <|> parseLet
        <|> parsePat
        <|> try parseLam
        <|> (ask >>= omLit <$$> contextPrimParser)
        <|> parseVar

    parseIf = omIf
        <$> (keyword "if"   *> exprParser)
        <*> (keyword "then" *> exprParser)
        <*> (keyword "else" *> exprParser)

    parseLet = do
        keyword "let"
        name <- nameParser <* token "="
        expr <- exprParser <* keyword "in"
        body <- exprParser
        pure (omLet name expr body)

    parseApp = do
        fun <- parseFun
        args <- components exprParser
        pure (omApp (fun:args))

    parseFun = try (parens exprParser)
        <|> omVar <$> ((word (withInitial (char '$')))
        <|> wordParser)

    parseLam = do
        name <- nameParser
        token "=>"
        body <- exprParser
        pure (omLam name body)

    parsePat = do
        keyword "match"
        expr <- exprParser
        clauses <- some parseClause
        optional (keyword "end")
        pure (omPat expr clauses)

    parseClause = do
        ParserContext{ contextPatternParser } <- ask
        token "|"
        names <- try (pure <$> wildcard) <|> contextPatternParser <|> do
            p <- wordParser
            ps <- optional args <#> fromMaybe []
            pure (p:ps)
        token "="
        expr <- exprParser
        pure (names, expr)
      where
        args = components (wildcard <|> wordParser)

wildcard :: Parser p Name
wildcard = token "_" $> wcard

parseVar :: Parser p (Om p)
parseVar = primFun
    <|> omVar <$> wordParser
  where
    primFun = char '$' *> (omPrim <$> nameParser)
