{-# LANGUAGE OverloadedStrings #-}
module Om.Prim.FunLang.Parser
  ( parser
  ) where

import Data.Functor (($>))
import Data.Text (Text, pack, unpack)
import Om.Lang
import Om.Lang.Parser
import Om.Prim.FunLang
import Text.Megaparsec hiding (token)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

--parseExpr =
--    token "()" $> omLit Unit

primParser :: Parser FunPrim FunPrim
primParser = parseTrue
    <|> parseFalse
    <|> parseUnit
    <|> parseChar
    <|> parseString
    <|> try parseFloat
    <|> parseIntegral
  where
    parseFloat    = Float <$> lexeme Lexer.float
    parseIntegral = Int <$> lexeme Lexer.decimal
    parseTrue     = keyword "true"  $> Bool True
    parseFalse    = keyword "false" $> Bool False
    parseChar     = Char <$> surroundedBy (token "'") printChar
    parseUnit     = token "()" $> Unit
    parseString   = lexeme (String . pack <$> chars)
    chars         = char '\"' *> manyTill Lexer.charLiteral (char '\"')

surroundedBy :: Parser p Text -> Parser p a -> Parser p a
surroundedBy parser = between parser parser

parser :: ParserContext FunPrim
parser = mempty
    { -- contextExprParser = parseExpr
      contextReserved =
        [ "true"
        , "false"
        ]
    , contextPrimParser = primParser
    }
