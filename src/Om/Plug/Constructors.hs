{-# LANGUAGE OverloadedStrings #-}
module Om.Plug.Constructors 
  ( constructorsPlugin
  ) where

import Data.Char (isUpper)
import Om.Eval
import Om.Plug
import Om.Util
import qualified Data.Text as Text

constructorsPlugin :: Plugin p (Eval p)
constructorsPlugin = plugin (Just constructorsLookupHook) Nothing

constructorsLookupHook :: LookupHook p (Eval p)
constructorsLookupHook var
  | hasUpperInitial var = pure (Just (Data var []))
  | otherwise = pure Nothing

hasUpperInitial :: Name -> Bool
hasUpperInitial "" = False
hasUpperInitial name = isUpper (Text.head name)
