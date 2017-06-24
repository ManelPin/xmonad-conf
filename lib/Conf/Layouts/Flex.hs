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

import Conf.Theme.Gaps (gaps, spacing)

import qualified XMonad.Layout.Accordion as Accordion
import qualified XMonad.Layout.Renamed as Renamed
import qualified XMonad.Layout.Simplest as Simplest
import qualified XMonad.Layout.Tabbed as Tabbed
import qualified XMonad.Layout.ResizableTile as ResizableTall
import qualified XMonad.Layout.ThreeColumns as ThreeColumns

import XMonad.Hooks.ManageDocks (avoidStruts)

import XMonad.Layout.BinarySpacePartition (emptyBSP)
import XMonad.Layout.Hidden (hiddenWindows)
import XMonad.Layout.NoFrillsDecoration (noFrillsDeco)
import XMonad.Layout.PerScreen (ifWider)
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
  $ ifWider smallMonResWidth wideLayouts standardLayouts

wideLayouts
  = gaps
  $ spacing
  $   (suffixed "Wide 3Col" $ ThreeColumns.ThreeColMid 1 (1 / 20) (1 / 2))
  ||| (trimSuffixed 1 "Wide BSP" $ hiddenWindows emptyBSP)

standardLayouts
  = gaps
  $ spacing
  $ (suffixed "Std 2/3" $ ResizableTall.ResizableTall 1 (1 / 20) (2 / 3) [])
  ||| (suffixed "Std 1/2" $ ResizableTall.ResizableTall 1 (1 / 20) (1 / 2) [])

suffixed n = Renamed.renamed [(Renamed.AppendWords n)]

trimNamed w n = Renamed.renamed [(Renamed.CutWordsLeft w), (Renamed.PrependWords n)]

trimSuffixed w n = Renamed.renamed [(Renamed.CutWordsRight w), (Renamed.AppendWords n)]

-- TODO: This is duplicated in Layouts.Tabs. Move to separate package
addTopBar = noFrillsDeco Tabbed.shrinkText Theme.topBar

smallMonResWidth = 1920 -- TODO: Move this elsewhere
