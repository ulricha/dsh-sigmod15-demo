{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE FlexibleInstances   #-}

-- | Order: A list-based data model
module Queries.SIGMOD where

import qualified Prelude      as P
import           Database.HDBC.ODBC

import           Database.DSH
import           Database.DSH.Compiler
import           Database.DSH.Backend.Sql

import           Schema.TPCH
import           Schema.AQuery

import           Queries.SIGMOD.Simple
import           Queries.SIGMOD.Order
import           Queries.SIGMOD.Layered
import           Queries.SIGMOD.Nested

tpch1Conn :: IO SqlBackend
tpch1Conn = sqlBackend <$> connectODBC "DSN=tpch1"

tradesConn :: IO SqlBackend
tradesConn = sqlBackend <$> connectODBC "trades"
