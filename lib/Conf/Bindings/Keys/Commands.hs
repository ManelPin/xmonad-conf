{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable,
  TypeSynonymInstances, MultiParamTypeClasses #-}

----------------------------------------------------------------------------
-- |
-- Module       : Conf.Bindings.Keys.Commands
-- Copyright    : (c) maddy@na.ai
-- License      : MIT
--
-- Maintainer   : maddy@na.ai
-- Stability    : unstable
-- Portability  : unportable
--
----------------------------------------------------------------------------
module Conf.Bindings.Keys.Commands
  ( commands
  ) where

import Conf.Bindings.Keys.Internal (subKeys)

import XMonad.Util.NamedActions (addName)

import qualified XMonad.Hooks.DebugStack as DebugStack

commands c = subKeys "Commands" c
  [ ( "M-<F2>", addName "Debug XMonad Stack (Current Workspace)"  DebugStack.debugStack)
  , ( "M-<F3>", addName "Debug XMonad Stack (All Workspaces)"     DebugStack.debugStackFull)
  ]
