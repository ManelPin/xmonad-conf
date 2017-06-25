{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable,
  TypeSynonymInstances, MultiParamTypeClasses #-}

----------------------------------------------------------------------------
-- |
-- Module       : Conf.Bindings.Keys.Launchers
-- Copyright    : (c) maddy@na.ai
-- License      : MIT
--
-- Maintainer   : maddy@na.ai
-- Stability    : unstable
-- Portability  : unportable
--
----------------------------------------------------------------------------
module Conf.Bindings.Keys.Launchers
  ( launchers
  ) where

import qualified Conf.Applications as Apps

import Conf.Bindings.Keys.Internal (subKeys)

import XMonad (spawn)
import XMonad.Util.NamedActions (addName)

launchers c = subKeys "Launchers" c
  [ ("M-<Space>",  addName "Launcher" $ spawn Apps.launcher)
  , ("M-<Return>", addName "Terminal" $ spawn Apps.terminal)
  , ("M-\\",       addName "Browser"  $ spawn Apps.browser)
  , ("M-s s",      addName "Cancel submap" $ return ())
  , ("M-s M-s",    addName "Cancel submap" $ return ())
  ]
