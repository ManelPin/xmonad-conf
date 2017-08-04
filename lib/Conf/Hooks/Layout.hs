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
import qualified Conf.Layouts.DS as Layouts.DS

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
  $ fullBarToggle
  $ mirrorToggle
  $ reflectToggle
  $ Layouts.Flex.flex
    ||| Layouts.Tabs.tabs
    ||| Layouts.DS.ds

showWorkspaceName = showWName' Theme.showWName

fullScreenToggle = mkToggle (single MultiToggle.Instances.FULL)
fullBarToggle    = mkToggle (single FULLBAR)
mirrorToggle     = mkToggle (single MultiToggle.Instances.MIRROR)
reflectToggle    = mkToggle (single Reflect.REFLECTX)

-- TODO: Move out of module?
data FULLBAR =
  FULLBAR
  deriving (Read, Show, Eq, XMonad.Typeable)

instance MultiToggle.Transformer FULLBAR XMonad.Window where
  transform FULLBAR x k = k barFull (\_ -> x)

barFull = avoidStruts $ Simplest.Simplest
