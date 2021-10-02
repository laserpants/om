{-# LANGUAGE OverloadedStrings #-}
module Om.Prim.BasicNats.Parser 
  ( parser
  ) where

import Data.Functor (($>))
import Om.Lang
import Om.Lang.Parser
import Om.Prim.BasicNats
import Om.Util
import Text.Megaparsec hiding (token)
import qualified Om.Prim.Basic.Parser as Basic
import qualified Text.Megaparsec.Char.Lexer as Lexer

primParser :: Parser BasicNatsPrim BasicNatsPrim
primParser =
    parseTrue
    <|> parseFalse
    <|> parseIntegral
  where
    parseIntegral = Int <$> lexeme Lexer.decimal
    parseTrue  = keyword "true"  $> Bool True
    parseFalse = keyword "false" $> Bool False

parseConstructors :: Parser p (Om p)
parseConstructors = parseSucc <|> parseZero
  where
    parseZero = omCon <$> "zero" 
    parseSucc = do
        keyword "succ" 
        omData "succ" . pure <$> parens exprParser

parsePattern :: Parser p [Name]
parsePattern = parseSucc <|> parseZero
  where
    parseZero = pure <$> "zero"
    parseSucc = do
        keyword "succ" 
        n <- parens (wildcard <|> nameParser)
        pure ["succ", n]

parser :: ParserContext BasicNatsPrim
parser = mempty
    { contextReserved =
        [ "succ"
        , "zero"
        , "true"
        , "false"
        ]
    , contextPrimParser = primParser
    , contextExprParser = parseConstructors
    , contextPatternParser = parsePattern
    }
