{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses, NoMonomorphismRestriction, FlexibleContexts #-}

----------------------------------------------------------------------------
-- |
-- Module       : Conf.Layouts.Tabs
-- Copyright    : (c) maddy@na.ai
-- License      : MIT
--
-- Maintainer   : maddy@na.ai
-- Stability    : unstable
-- Portability  : unportable
--
----------------------------------------------------------------------------
module Conf.Layouts.Tabs
  ( tabs
  ) where

import Conf.Layouts.Internal (addTopBar, named)

import qualified Conf.Theme as Theme

import Conf.Theme.Gaps (bigGaps)

import qualified XMonad.Layout.Simplest as Simplest
import qualified XMonad.Layout.Tabbed as Tabbed

import XMonad.Hooks.ManageDocks (avoidStruts)

tabs
  = named "T"
  $ avoidStruts
  $ addTopBar
  $ Tabbed.addTabs Tabbed.shrinkText Theme.tabbed
  $ bigGaps
  $ Simplest.Simplest
