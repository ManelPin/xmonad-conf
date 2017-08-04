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

import qualified Conf.Bindings.Keys.Internal as Internal

import Conf.Bindings.Keys.Launchers (launchers)
import Conf.Bindings.Keys.Layout (layout)
import Conf.Bindings.Keys.System (system)
import Conf.Bindings.Keys.Windows (windows)
import Conf.Bindings.Keys.Workspaces (workspaces)
import Conf.Bindings.Keys.Resize (resize)
import Conf.Bindings.Keys.Commands (commands)

import XMonad.Util.NamedActions ((^++^))

modMask = Internal.modMask

keys c
  =    system     c
  ^++^ launchers  c
  ^++^ windows    c
  ^++^ workspaces c
  ^++^ layout     c
  ^++^ resize     c
  ^++^ commands   c

