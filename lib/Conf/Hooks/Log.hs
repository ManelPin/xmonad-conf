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

import qualified XMonad.Prompt as Prompt

import qualified XMonad.Actions.CopyWindow as CopyWindow

import qualified XMonad.Hooks.DynamicLog as DynamicLog
import qualified XMonad.Hooks.EwmhDesktops as EwmhDesktops
import qualified XMonad.Hooks.FadeWindows as FadeWindows

import qualified XMonad.Util.NamedScratchpad as NamedScratchpad
import qualified XMonad.Util.Run as Run

log h
 = do
  FadeWindows.fadeWindowsLogHook Hooks.Fade.fade
  copies <- CopyWindow.wsContainingCopies
  let check ws
        | ws `elem` copies =
          DynamicLog.pad . DynamicLog.xmobarColor Colors.yellow Colors.red . DynamicLog.wrap "*" " " $ ws
        | otherwise = DynamicLog.pad ws
  EwmhDesktops.ewmhDesktopsLogHook
  DynamicLog.dynamicLogWithPP $ dyLog check h

dyLog c h =
  Prompt.def
    { DynamicLog.ppCurrent         = DynamicLog.xmobarColor Colors.active "" . DynamicLog.wrap "[" "]"
    , DynamicLog.ppTitle           = DynamicLog.xmobarColor Colors.active "" . DynamicLog.shorten 50
    , DynamicLog.ppVisible         = DynamicLog.xmobarColor Colors.base0 "" . DynamicLog.wrap "(" ")"
    , DynamicLog.ppUrgent          = DynamicLog.xmobarColor Colors.red "" . DynamicLog.wrap " " " "
    , DynamicLog.ppHidden          = c
    , DynamicLog.ppHiddenNoWindows = const ""
    , DynamicLog.ppSep             = DynamicLog.xmobarColor Colors.red Colors.blue "  :  "
    , DynamicLog.ppWsSep           = " "
    , DynamicLog.ppLayout          = DynamicLog.xmobarColor Colors.yellow ""
    , DynamicLog.ppOrder           = id
    , DynamicLog.ppOutput          = Run.hPutStrLn h
    , DynamicLog.ppExtras          = []
    , DynamicLog.ppSort            = fmap
                                      (NamedScratchpad.namedScratchpadFilterOutWorkspace .)
                                      (DynamicLog.ppSort Prompt.def)
    }
