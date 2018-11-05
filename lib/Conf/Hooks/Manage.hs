{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable,
  TypeSynonymInstances, MultiParamTypeClasses #-}

----------------------------------------------------------------------------
-- |
-- Module       : Conf.Hooks.Manage
-- Copyright    : (c) maddy@na.ai
-- License      : MIT
--
-- Maintainer   : maddy@na.ai
-- Stability    : unstable
-- Portability  : unportable
--
----------------------------------------------------------------------------
module Conf.Hooks.Manage
  ( manage
  ) where

import qualified Conf.Applications as Apps
import qualified Conf.NamedScratchpads as NamedScratchpads

import qualified XMonad

import qualified XMonad.Actions.SpawnOn as SpawnOn

import qualified XMonad.Hooks.ManageDocks as ManageDocks
import qualified XMonad.Hooks.ManageHelpers as ManageHelpers
import qualified XMonad.Hooks.InsertPosition as InsertPosition

import qualified XMonad.Layout.Fullscreen as Fullscreen

import qualified XMonad.StackSet as StackSet

import XMonad (className, doFloat, doIgnore, resource)
import XMonad.Hooks.InsertPosition (insertPosition)
import XMonad.Hooks.ManageHelpers
       (doCenterFloat, doFullFloat, isDialog, isFullscreen, isInProperty)
import XMonad.Util.NamedScratchpad (customFloating)
import XMonad ((<&&>), (<+>), (=?))
import XMonad.Hooks.ManageHelpers ((-?>))

manage :: XMonad.ManageHook
manage
  = manageSpecific
  <+> ManageDocks.manageDocks
  <+> Fullscreen.fullscreenManageHook
  <+> SpawnOn.manageSpawn
  <+> NamedScratchpads.manage

manageSpecific =
  ManageHelpers.composeOne
    [ className =? Apps.screencastClass -?> doCenterFloat
    -- Lines below need cleanup
    , resource =? "desktop_window" -?> doIgnore
    , resource =? "stalonetray" -?> doIgnore
    , resource =? "vlc" -?> doFloat
    , resource =? "feh" -?> centerFloat (3/4) (3/4)
    , resource =? "console" -?> tileBelowNoFocus
    , isRole =? gtkFile -?> forceCenterFloat
    -- , className =? Apps.passwordMgrClass -?> doIgnore
    -- , isRole =? "pop-up" -?> doCenterFloat -- TODO: Refine selection of which pop-ups to float
    , isDialog -?> doCenterFloat
    , isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH" -?> doCenterFloat -- TODO: Cleanup into 'isSplashScreen'
    , isFullscreen -?> doFullFloat
    , isBrowserDialog -?> forceCenterFloat
    , pure True -?> tileBelow
    , ManageHelpers.transience
    ]

isBrowserDialog
  = isDialog
  <&&> XMonad.className
  =? Apps.browserClass

gtkFile = "GtkFileChooserDialog"

isRole = XMonad.stringProperty "WM_WINDOW_ROLE"

tileBelow = insertPosition InsertPosition.Below InsertPosition.Newer

tileBelowNoFocus = insertPosition InsertPosition.Below InsertPosition.Older

-- TODO: Move this to another module
forceCenterFloat :: XMonad.ManageHook
forceCenterFloat = ManageHelpers.doFloatDep move
  where
    move :: StackSet.RationalRect -> StackSet.RationalRect
    move _ = StackSet.RationalRect x y w h
    w, h, x, y :: Rational
    w = 1 / 3
    h = 1 / 2
    x = (1 - w) / 2
    y = (1 - h) / 2

centerFloat width height
  = customFloating $ StackSet.RationalRect marginLeft marginTop width height
    where
      marginLeft = (1 - width) / 2
      marginTop = (1 - height) / 2
