{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable,
  TypeSynonymInstances, MultiParamTypeClasses #-}

----------------------------------------------------------------------------
-- |
-- Module       : Conf.Hooks.Log
-- Copyright    : (c) maddy@na.ai
-- License      : MIT
--
-- Maintainer   : maddy@na.ai
-- Stability    : unstable
-- Portability  : unportable
--
----------------------------------------------------------------------------
module Conf.Hooks.Log
  ( log
  ) where

import Prelude hiding (log)

import qualified Conf.Theme.Colors as Colors
import qualified Conf.Hooks.Fade as Hooks.Fade

import qualified XMonad

-- import qualified XMonad.Layout.IndependentScreens as IndependentScreens

import qualified XMonad.Hooks.DynamicLog as DynamicLog
import qualified XMonad.Hooks.DynamicBars as DynamicBars
import qualified XMonad.Hooks.EwmhDesktops as EwmhDesktops
import qualified XMonad.Hooks.FadeWindows as FadeWindows

import qualified XMonad.Util.NamedScratchpad as NamedScratchpad

log
 = do
  FadeWindows.fadeWindowsLogHook Hooks.Fade.fade
  EwmhDesktops.ewmhDesktopsLogHook
  DynamicBars.multiPP
    dyLogFocus
    dyLogUnfocus

-- dyLogUnfocus :: XMonad.ScreenId -> DynamicLog.PP
-- dyLogUnfocus n =
dyLogUnfocus :: DynamicLog.PP
dyLogUnfocus =
  -- IndependentScreens.marshallPP n
  DynamicLog.def
    -- Workspaces
    { DynamicLog.ppCurrent         = DynamicLog.xmobarColor Colors.green   "" . DynamicLog.wrap "[" "]"
    , DynamicLog.ppVisible         = DynamicLog.xmobarColor Colors.skyblue "" . DynamicLog.wrap "(" ")"
    , DynamicLog.ppUrgent          = DynamicLog.xmobarColor Colors.red     "" . DynamicLog.wrap " " " "
    , DynamicLog.ppHidden          = DynamicLog.xmobarColor Colors.blue    ""
    , DynamicLog.ppHiddenNoWindows = const ""

    -- Main
    , DynamicLog.ppTitle           = DynamicLog.xmobarColor Colors.green   "" . DynamicLog.shorten 50
    , DynamicLog.ppLayout          = DynamicLog.xmobarColor Colors.violet  ""

    -- Seps
    , DynamicLog.ppWsSep           = DynamicLog.xmobarColor Colors.base02  "" " / "
    , DynamicLog.ppSep             = DynamicLog.xmobarColor Colors.base02  "" "  |  "

    -- Mods
    , DynamicLog.ppOrder           = id
    , DynamicLog.ppExtras          = []
    , DynamicLog.ppSort            = fmap
                                      (NamedScratchpad.namedScratchpadFilterOutWorkspace .)
                                      (DynamicLog.ppSort DynamicLog.def)
    }

dyLogFocus :: DynamicLog.PP
dyLogFocus =
-- dyLogFocus :: XMonad.ScreenId -> DynamicLog.PP
-- dyLogFocus n =
  -- dyLogUnfocus n
  dyLogUnfocus
    { DynamicLog.ppCurrent = DynamicLog.xmobarColor Colors.orange "" . DynamicLog.wrap "[" "]"
    , DynamicLog.ppTitle   = DynamicLog.xmobarColor Colors.orange "" . DynamicLog.shorten 50
    }
