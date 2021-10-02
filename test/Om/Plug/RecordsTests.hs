{-# LANGUAGE OverloadedStrings #-}
module Om.Plug.RecordsTests where

import Data
import Data.Text (Text, unpack, pack)
import Om.Eval
import Om.Eval.Strict
import Om.Lang
import Om.Plug.Records
import Om.Prim.Basic
import Test.Hspec
import qualified Om.Plug.Records.Parser as Records
import qualified Om.Prim.Basic as Basic
import qualified Om.Prim.Basic.Parser as Basic

evalRecordsTests :: SpecWith ()
evalRecordsTests = do

    describe "Eval records" $ do

        testEvalBasic "{ one = 1, two = 2 }.two [2]"
            --
            -- .two({ one = 1, two = 2 })
            --
            (omApp [omVar ".two", example2])
            (Right (Value (Basic.Int 2)))

        testEvalBasic "{ one = 1, two = 2 }.one [1]"
            --
            -- .one(two, { one = 1, two = 2 })
            --
            (omApp [omVar ".one", example2])
            (Right (Value (Basic.Int 1)))

    describe "Eval records (pattern matching)" $ do

        testEvalBasic "Example #1"
            --
            -- let
            --   r =
            --     { one = 1, two = 2 }
            --   in
            --     match r
            --       | #(row) =
            --           match row
            --             | { one = o | _ } = o
            --
            (omLet "r"
                example2
                (omPat
                    (omVar "r")
                    [(["#", "row"],
                        omPat
                            (omVar "row")
                            [(["{one}", "o", wcard], omVar "o")]
                    )]))
            (Right (Value (Basic.Int 1)))

        testEvalBasic "Example #2"
            --
            -- let
            --   r =
            --     { two = 2, one = 1 }
            --   in
            --     match r
            --       | #(row) =
            --           match row
            --             | { one = o | _ } = o
            --
            (omLet "r"
                example3
                (omPat
                    (omVar "r")
                    [(["#", "row"],
                        omPat
                            (omVar "row")
                            [(["{one}", "o", wcard], omVar "o")]
                    )]))
            (Right (Value (Basic.Int 1)))

        testEvalBasic "Example #3"
            --
            -- let
            --   r =
            --     { one = 1, two = 2 }
            --   in
            --     match r
            --       | { one = o | two = t } = t
            --
            --               |||
            --                |
            --                v
            -- let
            --   r =
            --     { one = 1, two = 2 }
            --   in
            --     match r
            --       | #(row) =
            --           match row
            --             | { one = o | r1 } =
            --                 match r1
            --                   | { two = t } = t
            (omLet "r"
                example2
                (omPat
                    (omVar "r")
                    [(["#", "row"],
                        (omPat
                            (omVar "row")
                            [(["{one}", "o", "r1"],
                                (omPat
                                    (omVar "r1")
                                    [(["{two}", "t", "{}"], omVar "t")]
                                )
                            )])
                    )]))
            (Right (Value (Basic.Int 2)))

        testEvalBasic "Example #4"
            --
            -- let
            --   r =
            --     { two = 2, one = 1 }
            --   in
            --     match r
            --       | #(row) =
            --           match row
            --             | { one = o | r1 } =
            --                 match r1
            --                   | { two = t } = t
            (omLet "r"
                example3
                (omPat
                    (omVar "r")
                    [(["#", "row"],
                        (omPat
                            (omVar "row")
                            [(["{one}", "o", "r1"],
                                (omPat
                                    (omVar "r1")
                                    [(["{two}", "t", "{}"], omVar "t")]
                                )
                            )])
                    )]))
            (Right (Value (Basic.Int 2)))

        testEvalBasic "Example #5"
            --
            -- let
            --   r =
            --     { one = 1, two = 2 }
            --   in
            --     match r
            --       | { one = o | r } = r
            --
            --               |||
            --                |
            --                v
            -- let
            --   r =
            --     { one = 1, two = 2 }
            --   in
            --     match r
            --       | #(row) =
            --           match row
            --             | { one = o | r } = r
            (omLet "r"
                example2
                (omPat
                    (omVar "r")
                    [(["#", "row"],
                        (omPat
                            (omVar "row")
                            [(["{one}", "o", "r"], omVar "r")])
                    )]))
            (Right (Data "{two}" [Value (Basic.Int 2), Data "{}" []]))

        testEvalBasic "Example #6"
            --
            -- let
            --   r =
            --     { one = 1 }
            --   in
            --     match r
            --       | { one = o | r } = r
            --
            --               |||
            --                |
            --                v
            -- let
            --   r =
            --     { one = 1 }
            --   in
            --     match r
            --       | #(row) =
            --           match row
            --             | { one = o | r } = r
            (omLet "r"
                example4
                (omPat
                    (omVar "r")
                    [(["#", "row"],
                        (omPat
                            (omVar "row")
                            [(["{one}", "o", "r"], omVar "r")])
                    )]))
            (Right (Data "{}" []))

testEvalBasic :: Text -> Om BasicPrim -> Either Error (Result BasicPrim) -> SpecWith ()
testEvalBasic dscr om expect =
    it (unpack dscr) (expect == result)
  where
    result = evalExpr om basicPrelude recordsPlugin
