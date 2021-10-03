{-# LANGUAGE OverloadedStrings #-}
module Om.Prim.FunLang.Parser 
  ( parser
  ) where

import Data.Functor (($>))
import Data.Text (Text, pack, unpack)
import Om.Lang.Parser
import Om.Prim.FunLang
import Text.Megaparsec hiding (token)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

primParser :: Parser FunPrim FunPrim
primParser =
    parseTrue
    <|> parseFalse
    <|> parseIntegral
    <|> parseUnit
    -- TODO
--    <|> parseChar
--    <|> parseFloat
    <|> parseString
  where
    parseIntegral = Int <$> lexeme Lexer.decimal
    parseTrue   = keyword "true"  $> Bool True
    parseFalse  = keyword "false" $> Bool False
    parseUnit   = token "()" $> Unit
    parseString = lexeme (String . pack <$> chars)
    chars       = char '\"' *> manyTill Lexer.charLiteral (char '\"')

surroundedBy :: Parser p Text -> Parser p a -> Parser p a
surroundedBy parser = between parser parser

parser :: ParserContext FunPrim
parser = mempty
    { contextReserved =
        [ "true"
        , "false"
        ]
    , contextPrimParser = primParser
    }
