{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable,
  TypeSynonymInstances, MultiParamTypeClasses #-}

----------------------------------------------------------------------------
-- |
-- Module       : Conf.Workspaces
-- Copyright    : (c) maddy@na.ai
-- License      : MIT
--
-- Maintainer   : maddy@na.ai
-- Stability    : unstable
-- Portability  : unportable
--
----------------------------------------------------------------------------
module Conf.Workspaces
  ( workspaces
  , workspaceMap
  ) where

import Data.Map (Map)
import qualified Data.Map as Map

workspaceMap :: Map String String
workspaceMap = Map.fromList
  [ ("general", "GEN")
  , ("work", "WRK")
  ]

workspaces :: [String]
workspaces = Map.elems workspaceMap
