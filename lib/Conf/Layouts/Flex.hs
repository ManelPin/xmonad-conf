{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable,
  TypeSynonymInstances, MultiParamTypeClasses #-}

----------------------------------------------------------------------------
-- |
-- Module       : Conf.Layouts.Flex
-- Copyright    : (c) maddy@na.ai
-- License      : MIT
--
-- Maintainer   : maddy@na.ai
-- Stability    : unstable
-- Portability  : unportable
--
----------------------------------------------------------------------------
module Conf.Layouts.Flex
  ( flex
  ) where

import qualified Conf.Theme as Theme

import Conf.Layouts.Internal (addTopBar, trimNamed, suffixed, trimSuffixed)

import Conf.Theme.Gaps (gaps, spacing)

import qualified XMonad.Layout.Accordion as Accordion
import qualified XMonad.Layout.Simplest as Simplest
import qualified XMonad.Layout.Tabbed as Tabbed
import qualified XMonad.Layout.ResizableTile as ResizableTall

import XMonad.Hooks.ManageDocks (avoidStruts)

import XMonad.Layout.BinarySpacePartition (emptyBSP)
import XMonad.Layout.Hidden (hiddenWindows)
import XMonad.Layout.SubLayouts (subLayout)
import XMonad.Layout.WindowNavigation (windowNavigation)

import XMonad.Layout.LayoutCombinators ((|||))

flex
  = trimNamed 5 "Flex"
  $ avoidStruts
  $ windowNavigation
  $ addTopBar
  $ Tabbed.addTabs Tabbed.shrinkText Theme.tabbed
  $ subLayout []
    (Simplest.Simplest ||| Accordion.Accordion)
  $ layouts

layouts
  = gaps
  $ spacing
  $ (trimSuffixed 1 "Wide BSP" $ hiddenWindows emptyBSP)
  ||| (suffixed "Std 1/2" $ ResizableTall.ResizableTall 1 (1 / 20) (1 / 2) [])
