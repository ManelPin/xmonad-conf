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

import qualified Conf.Theme as Theme
--

import qualified XMonad.Layout.Renamed as Renamed
import qualified XMonad.Layout.Simplest as Simplest
import qualified XMonad.Layout.Tabbed as Tabbed

import XMonad.Layout.NoFrillsDecoration (noFrillsDeco)
import XMonad.Hooks.ManageDocks (avoidStruts)

tabs
  = named "Tabs"
  $ avoidStruts
  $ addTopBar
  $ Tabbed.addTabs Tabbed.shrinkText Theme.tabbed
  $ Simplest.Simplest

named n = Renamed.renamed [(Renamed.Replace n)]

-- TODO: This is duplicated in Layouts.Flex. Move to separate package
addTopBar = noFrillsDeco Tabbed.shrinkText Theme.topBar
