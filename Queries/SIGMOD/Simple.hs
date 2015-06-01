{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE OverloadedStrings   #-}

-- | Basic DSH Examples
module Queries.SIGMOD.Simple
    (
    ) where

import qualified Prelude      as P

import           Database.DSH
import           Database.DSH.Compiler
import           Database.DSH.Backend.Sql

import           Schema.TPCH

--------------------------------------------------------------------------------
-- Comprehensions

-- TPC-H customers (in some list order)
map1 :: Q [Customer]
map1 = customers

-- Haskell comprehensions are the principal tool to express queries
map2 :: Q [Text]
map2 = [ c_nameQ c | c <- customers ]

-- Higher-order combinators can be used as well
map3 :: Q [Text]
map3 = map (\c -> c_nameQ c) customers

map4 :: Q [Text]
map4 = map c_nameQ customers

-- Combining record selectors and tuple construction
map5 :: Q [(Text, Decimal)]
map5 = [ tup2 (c_nameQ c) (c_acctbalQ c) | c <- customers ]

--------------------------------------------------------------------------------
-- Filtering lists

-- A predicate on a comprehension
filter1 :: Q [Customer]
filter1 = [ c | c <- customers, c_mktsegmentQ c == "MACHINERY" ]

-- Filtering with a higher order combinator
filter2 :: Q [Customer]
filter2 = filter (\c -> c_mktsegmentQ c == "MACHINERY") customers

-- Two predicates on one comprehension
filter3 :: Q [Customer]
filter3 = [ c
          | c <- customers
          , c_mktsegmentQ c == "MACHINERY"
          , c_acctbalQ c > 0
          ]

--------------------------------------------------------------------------------
-- Basic joins

-- Joining orders and customers
join1 :: Q [(Customer, Order)]
join1 = [ tup2 c o | c <- customers, o <- orders, c_custkeyQ c == o_custkeyQ o ]

-- Joining orders and customers with additional predicates
join2 :: Q [(Text, Decimal)]
join2 = [ tup2 (c_nameQ c) (o_totalpriceQ o)
        | c <- customers
        , o <- orders
        , c_mktsegmentQ c == "MACHINERY"
        , c_acctbalQ c > 0
        , o_orderpriorityQ o == "5-LOW"
        , c_custkeyQ c == o_custkeyQ o
        ]

-- Joining three lists
join3 :: Q [(Customer, Order, LineItem)]
join3 = [ tup3 c o l
        | c <- customers
        , o <- orders
        , l <- lineitems
        , c_custkeyQ c == o_custkeyQ o
        , o_orderkeyQ o == l_orderkeyQ l
        ]

--------------------------------------------------------------------------------
-- Quantifiers

-- Existential quantification with a higher-order combinator
exists1 :: Q [Text]
exists1 = [ c_nameQ c
          | c <- customers
          , c_acctbalQ c > 9950.0
          , any (\n -> n_nationkeyQ n == c_nationkeyQ c)
                [ n | n <- nations, n_nameQ n == "CANADA" ]
          ]

-- Existential quantification with a first-order combinator
exists2 :: Q [Text]
exists2 = [ c_nameQ c
          | c <- customers
          , c_acctbalQ c > 9950.0
          , or [ n_nationkeyQ n == c_nationkeyQ c
               | n <- nations
               , n_nameQ n == "CANADA"
               ]
          ]

-- Existential quantifier with a literal list
exists3 :: Q [Text]
exists3 = [ c_nameQ c
          | c <- customers
          , n <- nations
          , c_nationkeyQ c == n_nationkeyQ n
          , n_nameQ n `elem` (toQ ["FRANCE", "GERMANY"])
          ]

-- Universal quantification with a higher-order combinator
forall1 :: Q [Text]
forall1 = [ c_nameQ c
          | c <- customers
          , c_acctbalQ c > 9950.0
          , all (\o -> o_orderstatusQ o == "F")
                [ o | o <- orders, o_custkeyQ o == c_custkeyQ c ]
          ]

-- Universal quantification with a first-order combinator
forall2 :: Q [Text]
forall2 = [ c_nameQ c
          | c <- customers
          , c_acctbalQ c > 9950.0
          , and [ o_orderstatusQ o == "F"
                | o <- orders, o_custkeyQ o == c_custkeyQ c
                ]
          ]

-- Combining quantifiers
forallexists :: Q [Text]
forallexists =
    [ c_nameQ c
    | c <- customers
    , c_acctbalQ c > 9950.0
    , and [ o_orderstatusQ o == "F"
          | o <- orders, o_custkeyQ o == c_custkeyQ c
          ]
    , any (\n -> n_nationkeyQ n == c_nationkeyQ c)
          [ n | n <- nations, n_nameQ n == "CANADA" ]
    ]

--------------------------------------------------------------------------------
