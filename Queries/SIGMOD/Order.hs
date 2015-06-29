{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ViewPatterns        #-}

-- | Order: A list-based data model
module Queries.SIGMOD.Order
    ( order1, order2, order3, order4
    , nationsTopCustomers
    , pairTopCustomers
    , regionsTopCustomers
    , bestProfit
    ) where

import qualified Prelude                     as P

import           Database.DSH                hiding (topK)
import           Database.DSH.Backend.Sql
import           Database.DSH.Compiler

import           Queries.TPCH.BuildingBlocks
import           Schema.AQuery
import           Schema.TPCH

--------------------------------------------------------------------------------
-- Some simple order-aware queries

-- A base table scan returns elements in some list order
order1 :: Q [Customer]
order1 = customers

-- Change list order by sorting
order2 :: Q [Customer]
order2 = sortWith (\c -> c_acctbalQ c) customers

-- Positional operators
order3 :: Q [Customer]
order3 = take 5 (sortWith (\c -> c_acctbalQ c) customers)

-- List positions can be materialized
order4 :: Q [(Text, Integer)]
order4 = number [ n_nameQ n | n <- sortWith (\n -> n_nameQ n) nations ]

--------------------------------------------------------------------------------
-- An ordered view on TPC-H

-- Generic combinator: Return the top /k/ elements based on a
-- supplied ordering criterion
topK :: (QA a, TA b, QA b, Ord b) => Integer -> (Q a -> Q b) -> Q [a] -> Q [a]
topK k f as = take (toQ k) $ map fst $ sortWith snd $ [ tup2 a (f a) | a <- as ]

-- Domain-specific combinator: Produce top k customers (by number of
-- orders) from one country
nationsTopCustomers :: Integer -> Text -> Q [(Text, Decimal)]
nationsTopCustomers k n =
    [ pair (c_nameQ c) (c_acctbalQ c)
    | c <- topK k (length . custOrders)
                  [ c | c <- customers, c `custFromNation` n ]
    ]

-- Compare the customer charts of two countries
pairTopCustomers :: Q [((Text, Decimal), (Text, Decimal))]
pairTopCustomers = zip (nationsTopCustomers 10 "FRANCE")
                       (nationsTopCustomers 10 "GERMANY")

-- Nested and ordered data: for each nation in a region, determine the
-- nations' top customers by number of orders.
regionsTopCustomers :: Text -> Integer -> Q [(Text, [Customer])]
regionsTopCustomers rn k =
    [ pair (n_nameQ n)
           (topK k (length . custOrders)
                   [ c | c <- customers, c_nationkeyQ c == n_nationkeyQ n ])
    | r <- regions
    , r_nameQ r == toQ rn
    , n <- regionNations r
    ]

--------------------------------------------------------------------------------
-- Running aggregates

-- rolling minimum (mins [3,4,1,7] = [3,3,1,1])
mins :: (QA a, TA a, Ord a) => Q [a] -> Q [a]
mins xs = [ minimum [ y | (view -> (y, j)) <- number xs, j <= i ]
          | (view -> (x, i)) <- number xs
          ]

-- margin ˆ= current value - minimum value up to now
margins :: (Ord a, Num (Q a), QA a, TA a) => Q [a] -> Q [a]
margins xs = [ x - y | (view -> (x,y)) <- zip xs (mins xs) ]

-- our profit is the maximum margin obtainable
profit :: (Ord a, Num a, Num (Q a), QA a, TA a) => Q [a] -> Q a
profit xs = maximum (margins xs)

-- best profit obtainable for stock on given date
-- bestProfit 5 4
bestProfit :: Integer -> Integer -> Q Double
bestProfit stock date =
    profit [ t_priceQ t
           | t <- sortWith t_timestampQ trades
           , t_tidQ t == toQ stock
           , t_tradeDateQ t == toQ date
           ]
