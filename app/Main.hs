{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
module Main where

import Control.Monad.Reader
import Data.Maybe (fromJust)
import Data.Tuple.Extra (first, fst3, snd3, thd3, first3, second3, third3)
import Debug.Trace
import Om.Eval
import Om.Eval.Strict
import Om.Lang
import Om.Plug
import Om.Plug.Constructor
import Om.Plug.Pattern
import Om.Prim
import Om.Prim.Basic
import Om.Prim.BasicNats
import Om.Util
import qualified Data.Map.Strict as Map
import qualified Om.Prim.BasicNats as BasicNats

natsPlugin :: (PrimType p Integer) => Plugin p (Eval p)
natsPlugin = plugin (Just natsLookupHook) Nothing

natsLookupHook :: (PrimType p Integer) => LookupHook p (Eval p)
natsLookupHook var

  | "succ" == var = do
      let closr body = pure (Closure "?0" body mempty)
      Just <$$> closr $ do
          env <- ask <#> fst3
          fromJust (Map.lookup "?0" env) >>= \case
              Value n -> do
                  let z = fromJust (fromPrim n)
                  pure (Value (toPrim (z + 1)))

  | "zero" == var =
      pure (Just (Value (toPrim 0)))

  | otherwise = pure Nothing


-- succ(succ(zero))
--
example5 =
    omApp
        [ omVar "succ"
        , omApp
            [ omVar "succ"
            , omVar "zero"
            ]
        ]


-- zero
--
example6 = omVar "zero"


example7 =
    (omLet "m"
        (omApp
            [ omVar "succ"
            , omApp
                [ omVar "succ"
                , omVar "zero"
                ]
            ])
        (omLet "n"
            (omApp 
                [ omVar "succ"
                , omApp
                    [ omVar "succ"
                    , omApp
                        [ omVar "succ"
                        , omVar "zero"
                        ]
                    ]
                ])
            (omApp 
                [ omPrim "add"
                , omVar "m"
                , omVar "n"
                ])))



test123 :: Om BasicNatsPrim -> IO ()
test123 om =
    print (result <#> toString)
  where
    result = evalExpr om basicNatsPrelude (constructorPlugin <> recordPlugin <> natsPlugin)



main :: IO ()
main = pure ()
