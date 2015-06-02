{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ViewPatterns        #-}

module Queries.SIGMOD.Nested
    ( nationCount
    , allRegionsNations
    , shippingDelay
    , expectedRevenueFor
    ) where

import qualified Data.Text                   as T
import qualified Prelude                     as P

import           Database.DSH
import           Database.DSH.Backend.Sql
import           Database.DSH.Compiler

import           Schema.TPCH

--------------------------------------------------------------------------------

-- Constructing and flattening (via aggregation) nested data
nationCount :: Q [(Text, Integer)]
nationCount =
    [ pair (r_nameQ r)
           (length [ n | n <- nations, n_regionkeyQ n == r_regionkeyQ r ])
    | r <- regions
    ]

-- A query that constructs nested data
allRegionsNations :: Q [(Text, [Text])]
allRegionsNations =
    [ pair (r_nameQ r)
           [ n_nameQ n | n <- nations, n_regionkeyQ n == r_regionkeyQ r ]
    | r <- regions
    ]

--------------------------------------------------------------------------------
-- Complex reports with nested results

-- Return all orders with a given status
ordersWithStatus :: Text -> Q [Order]
ordersWithStatus status =
    [ o | o <- orders, o_orderstatusQ o == toQ status ]

-- Has the order been ordered by a given customer?
orderedBy :: Q Order -> Q Customer -> Q Bool
orderedBy o c = o_custkeyQ o == c_custkeyQ c

-- | Does the customer originate from the given nation?
custFromNation :: Q Customer -> Text -> Q Bool
custFromNation c nn =
    or [ n_nameQ n == toQ nn && n_nationkeyQ n == c_nationkeyQ c
       | n <- nations
       ]

-- The discounted price of an item
discPrice :: Q LineItem -> Q Decimal
discPrice l = l_extendedpriceQ l * (1 - l_discountQ l)

-- The price of an item after taxes
chargedPrice :: Q LineItem -> Q Decimal
chargedPrice l = discPrice l * (1 + l_taxQ l)

-- The total price of a number of items.
revenue :: Q [LineItem] -> Q Decimal
revenue ls = sum $ map discPrice ls

-- Revenue from a given order.
orderRevenue :: Q Order -> Q Decimal
orderRevenue o = revenue $ orderItems o

-- All lineitems of one particular order
orderItems :: Q Order -> Q [LineItem]
orderItems o = [ l | l <- lineitems, l_orderkeyQ l == o_orderkeyQ o ]

-- Expected revenue report
expectedRevenueFor :: Text -> Q [(Text, [(Day, Decimal)])]
expectedRevenueFor nationName =
    [ pair (c_nameQ c) [ pair (o_orderdateQ o) (orderRevenue o)
                       | o <- ordersWithStatus "P"
                       , o `orderedBy` c
                       ]
    | c <- customers
    , c `custFromNation` nationName
    , any (\o -> o `orderedBy` c) (ordersWithStatus "P")
    ]

--------------------------------------------------------------------------------
-- A simpler running example

-- For all orders, compute item quantities and average shipping time
shippingDelay :: Q [(Integer, [Decimal], Double)]
shippingDelay =
    [ let ls = orderItems o
      in tup3 (o_orderkeyQ o)
              [ l_quantityQ l | l <- sortWith l_shipdateQ ls ]
              (avg [ integerToDouble $ diffDays (o_orderdateQ o) (l_shipdateQ l) | l <- ls ])
    | o <- orders
    , o_totalpriceQ o > 500000
    ]
