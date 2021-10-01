{-# LANGUAGE OverloadedStrings #-}
module Om.Prim.BasicNats.Parser where

import Data.Functor (($>))
import Om.Lang.Parser
import Om.Prim.BasicNats
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

parserContext :: ParserContext BasicNatsPrim
parserContext = mempty
    { contextReserved =
        [ "succ"
        , "zero"
        , "true"
        , "false"
        ]
    , contextConstructors = 
        [ "succ"
        , "zero"
        ]
    , contextPrimParser = primParser
    }
