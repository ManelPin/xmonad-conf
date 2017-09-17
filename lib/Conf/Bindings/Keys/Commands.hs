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

import XMonad (spawn)
import XMonad.Util.NamedActions (addName)

commands c = subKeys "Commands" c
  [ ( "M-e",   addName "API Reload (Skip Tests)"  $ spawn "$HOME/bin/sigsend -n")
  , ( "M-S-r", addName "API Reload (Test All)"    $ spawn "$HOME/bin/sigsend -a")
  , ( "M-r",   addName "API Reload (Test Latest)" $ spawn "$HOME/bin/sigsend -d")
  , ( "M-w",   addName "API Reload (Lock Latest)" $ spawn "$HOME/bin/sigsend -l")
  ]
