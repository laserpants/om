{-# LANGUAGE OverloadedStrings #-}
module Om.Plug.Constructors.Parser
  ( parser
  ) where

import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Data.Text (cons)
import Om.Lang
import Om.Lang.Parser
import Om.Prim.BasicNats
import Om.Util
import Text.Megaparsec hiding (token)
import Text.Megaparsec.Char
import qualified Om.Prim.Basic.Parser as Basic
import qualified Text.Megaparsec.Char.Lexer as Lexer

parseData :: Parser p (Om p)
parseData = do
    uppr <- upperChar
    rest <- wordParser
    args <- optional (components exprParser)
    pure (omData (cons uppr rest) (fromMaybe [] args))

parser :: ParserContext p
parser = mempty
    { contextExprParser = parseData
    }
