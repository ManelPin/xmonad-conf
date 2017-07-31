{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable,
  TypeSynonymInstances, MultiParamTypeClasses #-}

----------------------------------------------------------------------------
-- |
-- Module       : Conf.Hooks.Startup
-- Copyright    : (c) maddy@na.ai
-- License      : MIT
--
-- Maintainer   : maddy@na.ai
-- Stability    : unstable
-- Portability  : unportable
--
----------------------------------------------------------------------------
module Conf.Hooks.Startup
  ( startup
  ) where

import qualified Conf.Applications as Apps
-- import qualified Conf.Hooks.StatusBars as StatusBars

import qualified XMonad
-- import qualified XMonad.Hooks.DynamicBars as DynamicBars
import qualified XMonad.Util.Run as Run
import qualified XMonad.Util.Cursor as Cursor

startup
 = do
  Cursor.setDefaultCursor Cursor.xC_left_ptr
  -- StatusBars.startupHook
  return ()
