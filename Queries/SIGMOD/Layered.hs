{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ViewPatterns        #-}

module Queries.SIGMOD.Layered
    ( fkjoin
    , q22, q22Default
    , q13, q13Default
    ) where

import qualified Data.Text                   as T
import qualified Prelude                     as P

import           Database.DSH                hiding (concat, elem, groupAggr,
                                              take)
import           Database.DSH.Backend.Sql
import           Database.DSH.Compiler

import           Queries.TPCH.BuildingBlocks hiding (custOrders)
import           Schema.TPCH

--------------------------------------------------------------------------------
-- Comprehensive power: Defining generic query abstractions from
-- built-ins, domain-independent

-- Working with materialized list positions
take :: QA a => Q Integer -> Q [a] -> Q [a]
take n xs = [ x | (view -> (x, i)) <- number xs, i <= n ]

-- List membership
elem :: (QA a, TA a, Eq a) => Q a -> Q [a] -> Q Bool
elem x xs = or [ x == x' | x' <- xs ]

-- Flattening lists
concat :: QA a => Q [[a]] -> Q [a]
concat xss = [ x | xs <- xss, x <- xs ]

-- rolling minimum (mins [3,4,1,7] = [3,3,1,1])
mins :: (QA a, TA a, Ord a) => Q [a] -> Q [a]
mins xs = [ minimum [ y | (view -> (y, j)) <- number xs, j <= i ]
          | (view -> (x, i)) <- number xs
          ]

-- Generic combinator: Return the top /k/ elements based on a
-- supplied ordering criterion
topK :: (QA a, TA b, QA b, Ord b) => Integer -> (Q a -> Q b) -> Q [a] -> Q [a]
topK k f as = take (toQ k) $ map fst $ sortWith snd $ [ tup2 a (f a) | a <- as ]

-- Higher-order abstractions: Combine grouping and aggregation
groupAggr :: (QA a, QA b, QA c, QA k, Ord k, TA k)
          => (Q a -> Q k)    -- ^ The grouping key
          -> (Q a -> Q b)    -- ^ The aggregate input
          -> (Q [b] -> Q c)  -- ^ The aggregate function
          -> Q [a]           -- ^ The input list
          -> Q [(k, c)]
groupAggr k p agg as =
    map (\kg -> pair (fst kg) (agg $ map p $ snd kg)) (groupWithKey k as)

--------------------------------------------------------------------------------
-- Building complex queries from simple, reusable, domain-specific abstractions

-- Abstracting over a foreign key relationship: One customers' orders
custOrders :: Q Customer -> Q [Order]
custOrders c = [ o | o <- orders, c_custkeyQ c == o_custkeyQ o ]

-- A simple foreign-key join
fkjoin :: Q [(Customer, Order)]
fkjoin = [ pair c o | c <- customers, o <- custOrders c ]

--------------------------------------------------------------------------------
-- TPC-H Q22

-- Average customer account balance
avgBalance :: Q [Customer] -> Q Decimal
avgBalance cs = avg [ c_acctbalQ c | c <- cs , c_acctbalQ c > 0]

-- Identify potential customers
potentialCustomers :: Q [Customer] -> Q [Customer]
potentialCustomers cs =
    [ c
    | c <- cs
    , c_acctbalQ c > avgBalance cs
    , null (custOrders c)
    ]

-- The national phone prefix of a customer
countryCodeOf :: Q Customer -> Q Text
countryCodeOf c = subString 1 2 (c_phoneQ c)

-- Does the customer live in one of the given countries (by phone prefix)?
livesIn :: Q Customer -> [Text] -> Q Bool
livesIn c countries = countryCodeOf c `elem` toQ countries

-- TPC-H query Q22
q22 :: [Text] -> Q [(Text, Integer, Decimal)]
q22 countries =
    sortWith (\(view -> (country, _, _)) -> country)
    [ tup3 country (length custs) (sum (map c_acctbalQ custs))
    | (view -> (country, custs)) <- groupWithKey countryCodeOf pots
    ]
  where
    pots = potentialCustomers [ c | c <- customers, c `livesIn` countries ]

-- | TPC-H Q22 with standard validation parameters
q22Default :: Q [(Text, Integer, Decimal)]
q22Default = q22 ["13", "31", "23", "29", "30", "18", "17"]

--------------------------------------------------------------------------------
-- TPC-H Q13. Note that we replace the LEFT OUTER JOIN and grouping
-- with a nestjoin, to include those customers with an empty list of
-- (relevant) orders.

-- | Nested data instead of OUTER JOINs: Compute number of orders per
-- customer, including those that have not placed any orders.
ordersPerCustomer :: Text -> Q [(Integer, Integer)]
ordersPerCustomer pat =
    [ tup2 (c_custkeyQ c)
           (length $ filter (\o -> o_commentQ o `notLike` (toQ pat))
                   $ custOrders c)
    | c <- customers
    ]

-- | TPC-H Q13: Distribution of orders per customer, including
-- customers without orders.
q13 :: Text -> Text -> Q [(Integer, Integer)]
q13 w1 w2 =
    sortWith (\(view -> (c_count, custdist)) -> pair (custdist * (-1))
                                                     (c_count * (-1)))
    $ groupAggr snd id length
    $ ordersPerCustomer pat
  where
    pat = foldr T.append "" ["%", w1, "%", w2, "%"]

-- | TPC-H Query Q13 with standard validation parameters
q13Default :: Q [(Integer, Integer)]
q13Default = q13 "special" "requests"
