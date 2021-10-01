module Om.Prim.Basic.Parser where

import Data.Functor (($>))
import Om.Lang.Parser
import Om.Prim.Basic
import Text.Megaparsec hiding (token)
import qualified Text.Megaparsec.Char.Lexer as Lexer

primParser :: Parser BasicPrim
primParser = 
    parseTrue
    <|> parseFalse
    <|> parseIntegral
  where
    parseIntegral = Int <$> lexeme Lexer.decimal
    parseTrue  = keyword "true"  $> Bool True
    parseFalse = keyword "false" $> Bool False
