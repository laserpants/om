{-# LANGUAGE OverloadedStrings #-}
module Om.Prim.Basic.Parser 
  ( parser
  ) where

import Data.Functor (($>))
import Om.Lang.Parser
import Om.Prim.Basic
import Text.Megaparsec hiding (token)
import qualified Text.Megaparsec.Char.Lexer as Lexer

primParser :: Parser BasicPrim BasicPrim
primParser =
    parseTrue
    <|> parseFalse
    <|> parseIntegral
  where
    parseIntegral = Int <$> lexeme Lexer.decimal
    parseTrue  = keyword "true"  $> Bool True
    parseFalse = keyword "false" $> Bool False

parser :: ParserContext BasicPrim
parser = mempty
    { contextReserved =
        [ "true"
        , "false"
        ]
    , contextPrimParser = primParser
    }
