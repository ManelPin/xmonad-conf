{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses, NoMonomorphismRestriction, FlexibleContexts #-}

----------------------------------------------------------------------------
-- |
-- Module       : Conf.Layouts.CDT
-- Copyright    : (c) maddy@na.ai
-- License      : MIT
--
-- Maintainer   : maddy@na.ai
-- Stability    : unstable
-- Portability  : unportable
--
----------------------------------------------------------------------------
module Conf.Layouts.CDT
  ( cdt
  , name
  ) where

import Conf.Layouts.Internal (addTopBar, named)

import qualified Conf.Theme as Theme

import Conf.Theme.Gaps (gaps, spacing)

import qualified XMonad.Layout.ComboP as ComboP
import qualified XMonad.Layout.Simplest as Simplest
import qualified XMonad.Layout.Tabbed as Tabbed
import qualified XMonad.Layout.TwoPaneV as TwoPaneV

import XMonad.Hooks.ManageDocks (avoidStruts)

import XMonad.Layout.WindowNavigation (windowNavigation)

name = "CDT" -- Chromium-Devtools Tabbed

cdt
  = named name
  $ avoidStruts
  $ windowNavigation
  $ ComboP.combineTwoP
    container
    browser
    others
    props

container
  = gaps
  $ spacing
  $ TwoPaneV.TwoPaneV (2/100) (8/10)

browser
  = addTopBar
  $ Tabbed.addTabs Tabbed.shrinkText Theme.tabbed
  $ Simplest.Simplest

others
  = addTopBar
  $ Tabbed.addTabs Tabbed.shrinkText Theme.tabbed
  $ Simplest.Simplest

props =
  ComboP.And
    (ComboP.ClassName "Chromium")
    (ComboP.Role "browser") -- Only allow Chromium "browser" wins - DevTools wins have role "pop-up"
