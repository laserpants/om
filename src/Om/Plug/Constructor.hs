{-# LANGUAGE OverloadedStrings #-}
module Om.Plug.Constructor 
  ( constructorPlugin
  ) where

import Data.Char (isUpper)
import Om.Eval
import Om.Plug
import Om.Util
import qualified Data.Text as Text

constructorPlugin :: Plugin p (Eval p)
constructorPlugin = plugin (Just constructorLookupHook) Nothing

constructorLookupHook :: LookupHook p (Eval p)
constructorLookupHook var
  | hasUpperInitial var = pure (Just (Data var []))
  | otherwise = pure Nothing

hasUpperInitial :: Name -> Bool
hasUpperInitial "" = False
hasUpperInitial name = isUpper (Text.head name)
