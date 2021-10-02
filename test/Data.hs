{-# LANGUAGE OverloadedStrings #-}
module Data where

import Om.Lang
import Om.Lang.Parser
import Om.Prim.Basic
import Om.Prim.BasicNats
import qualified Om.Plug.Constructors.Parser as Constructors
import qualified Om.Plug.Records.Parser as Records
import qualified Om.Prim.Basic as Basic
import qualified Om.Prim.Basic.Parser as Basic
import qualified Om.Prim.BasicNats as BasicNats
import qualified Om.Prim.BasicNats.Parser as BasicNats

-- Cons(1, Cons(2, (Cons 3, Nil)))
example1 :: Om BasicPrim
example1 =
    omData "Cons"
        [ omLit (Basic.Int 1)
        , omData "Cons"
            [ omLit (Basic.Int 2)
            , omData "Cons"
                [ omLit (Basic.Int 3)
                , omData "Nil" []
                ]
            ]
        ]

-- { one = 1, two = 2 }
example2 :: Om BasicPrim
example2 =
    omData "#"
        [ omData "{one}"
            [ omLit (Basic.Int 1)
            , omData "{two}"
                [ omLit (Basic.Int 2)
                , omData "{}" []
                ]
            ]
        ]

-- { two = 2, one = 1 }
example3 :: Om BasicPrim
example3 =
    omData "#"
        [ omData "{two}"
            [ omLit (Basic.Int 2)
            , omData "{one}"
                [ omLit (Basic.Int 1)
                , omData "{}" []
                ]
            ]
        ]

-- { one = 1 }
example4 :: Om BasicPrim
example4 =
    omData "#"
        [ omData "{one}"
            [ omLit (Basic.Int 1)
            , omData "{}" []
            ]
        ]

exampleContext1 :: ParserContext BasicPrim
exampleContext1 = Basic.parser <> Records.parser <> Constructors.parser

exampleContext2 :: ParserContext BasicNatsPrim
exampleContext2 = BasicNats.parser <> Records.parser
