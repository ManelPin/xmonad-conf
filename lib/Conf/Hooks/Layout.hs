{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable,
  TypeSynonymInstances, MultiParamTypeClasses #-}

----------------------------------------------------------------------------
-- |
-- Module       : Conf.Hooks.Layout
-- Copyright    : (c) maddy@na.ai
-- License      : MIT
--
-- Maintainer   : maddy@na.ai
-- Stability    : unstable
-- Portability  : unportable
--
----------------------------------------------------------------------------
module Conf.Hooks.Layout
  ( layout
  ) where

import qualified Conf.Theme as Theme
import qualified Conf.Layouts.Flex as Layouts.Flex
import qualified Conf.Layouts.Tabs as Layouts.Tabs
import qualified Conf.Layouts.DST as Layouts.DST

import qualified XMonad

import qualified XMonad.Layout.MultiToggle as MultiToggle
import qualified XMonad.Layout.MultiToggle.Instances as MultiToggle.Instances
import qualified XMonad.Layout.Reflect as Reflect
import qualified XMonad.Layout.Simplest as Simplest

import XMonad.Hooks.ManageDocks (avoidStruts)

import XMonad.Layout.Fullscreen (fullscreenFloat)
import XMonad.Layout.MultiToggle (mkToggle, single)
import XMonad.Layout.ShowWName (showWName')

import XMonad.Layout.LayoutCombinators ((|||))

layout
  = showWorkspaceName
  $ fullscreenFloat
  $ fullScreenToggle
  $ Layouts.Flex.flex
    ||| Layouts.Tabs.tabs
    ||| Layouts.DST.dst


showWorkspaceName = showWName' Theme.showWName

fullScreenToggle = mkToggle (single MultiToggle.Instances.FULL)
