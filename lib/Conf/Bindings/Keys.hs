{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable,
  TypeSynonymInstances, MultiParamTypeClasses #-}

----------------------------------------------------------------------------
-- |
-- Module       : Conf.Bindings.Keys
-- Copyright    : (c) maddy@na.ai
-- License      : MIT
--
-- Maintainer   : maddy@na.ai
-- Stability    : unstable
-- Portability  : unportable
--
----------------------------------------------------------------------------
module Conf.Bindings.Keys
  ( modMask
  , keys
  ) where

import Conf.Bindings.Keys.Launchers (launchers)
import Conf.Bindings.Keys.Layout (layout)
import Conf.Bindings.Keys.System (system)
import Conf.Bindings.Keys.Windows (windows)
import Conf.Bindings.Keys.Workspaces (workspaces)
import Conf.Bindings.Keys.Resize (resize )

import qualified XMonad

import XMonad.Util.NamedActions ((^++^))

modMask = XMonad.mod4Mask

keys c
  =    system     c
  ^++^ launchers  c
  ^++^ windows    c
  ^++^ workspaces c
  ^++^ layout     c
  ^++^ resize     c

