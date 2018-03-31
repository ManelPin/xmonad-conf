{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses, NoMonomorphismRestriction, FlexibleContexts #-}

----------------------------------------------------------------------------
-- |
-- Module       : Conf.Layouts.DST
-- Copyright    : (c) maddy@na.ai
-- License      : MIT
--
-- Maintainer   : maddy@na.ai
-- Stability    : unstable
-- Portability  : unportable
--
----------------------------------------------------------------------------
module Conf.Layouts.DST
  ( dst
  , name
  ) where

import Conf.Layouts.Internal (addTopBar, named)

import qualified Conf.Theme as Theme
import qualified Conf.Applications as Apps

import Conf.Theme.Gaps (gaps, spacing)

import qualified XMonad.Layout.ComboP as ComboP
import qualified XMonad.Layout.Simplest as Simplest
import qualified XMonad.Layout.Tabbed as Tabbed
import qualified XMonad.Layout.TwoPane as TwoPane

import XMonad.Hooks.ManageDocks (avoidStruts)

import XMonad.Layout.WindowNavigation (windowNavigation)

name = "DST" -- Dirciple Tabbed

dst
  = named name
  $ avoidStruts
  $ windowNavigation
  $ ComboP.combineTwoP
    container
    dirciple
    tabs
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

props = ComboP.ClassName Apps.dircipleClass
