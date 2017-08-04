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

import qualified XMonad
import qualified XMonad.Util.Cursor as Cursor

startup :: XMonad.X ()
startup
 = do
  Cursor.setDefaultCursor Cursor.xC_left_ptr
  return ()
