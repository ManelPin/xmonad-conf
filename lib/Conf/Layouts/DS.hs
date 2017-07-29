{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses, NoMonomorphismRestriction, FlexibleContexts #-}

----------------------------------------------------------------------------
-- |
-- Module       : Conf.Layouts.DS
-- Copyright    : (c) maddy@na.ai
-- License      : MIT
--
-- Maintainer   : maddy@na.ai
-- Stability    : unstable
-- Portability  : unportable
--
----------------------------------------------------------------------------
module Conf.Layouts.DS
  ( ds
  ) where

import Conf.Layouts.Internal (addTopBar, named)

import qualified Conf.Theme as Theme
import qualified Conf.Applications as Apps

import Conf.Theme.Gaps (gaps, spacing)

import qualified XMonad.Layout.ComboP as ComboP
import qualified XMonad.Layout.Renamed as Renamed
import qualified XMonad.Layout.Simplest as Simplest
import qualified XMonad.Layout.Tabbed as Tabbed
import qualified XMonad.Layout.TwoPane as TwoPane

import XMonad.Hooks.ManageDocks (avoidStruts)

import XMonad.Layout.WindowNavigation (windowNavigation)


--
import XMonad.Layout.NoFrillsDecoration (noFrillsDeco)
import qualified XMonad.Layout.Renamed as Renamed

--

ds
  = named "DS Tabbed"
  $ avoidStruts
  $ windowNavigation
  $ ComboP.combineTwoP
    container
    tabs
    dirciple
    props

container
  = gaps
  $ spacing
  $ TwoPane.TwoPane (2/100) (2/10)

dirciple
  = addTopBar
  $ Tabbed.addTabs Tabbed.shrinkText Theme.tabbed
  $ Simplest.Simplest

tabs
  = addTopBar
  $ Tabbed.addTabs Tabbed.shrinkText Theme.tabbed
  $ Simplest.Simplest
  -- TODO: Make this group a subgroup which splits vertically?

props = ComboP.ClassName Apps.dircipleClass

-- addTopBar = noFrillsDeco Tabbed.shrinkText Theme.topBar
