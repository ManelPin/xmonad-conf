{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable,
  TypeSynonymInstances, MultiParamTypeClasses #-}

----------------------------------------------------------------------------
-- |
-- Module       : Conf.Bindings.Keys.System
-- Copyright    : (c) maddy@na.ai
-- License      : MIT
--
-- Maintainer   : maddy@na.ai
-- Stability    : unstable
-- Portability  : unportable
--
----------------------------------------------------------------------------
module Conf.Bindings.Keys.System
  ( system
  ) where

import qualified XMonad

import Conf.Theme (hotPrompt)
import Conf.Bindings.Keys.Internal (subKeys)

import System.Exit (exitWith, ExitCode(ExitSuccess))

import XMonad (spawn, io)
import XMonad.Prompt.ConfirmPrompt (confirmPrompt)
import XMonad.Util.NamedActions (addName)

system c = subKeys "System" c
  [ ( "M-q",   addName "Restart XMonad"           restart)
  , ( "M-C-q", addName "Rebuild & restart XMonad" rebuild_restart)
  , ( "M-S-q", addName "Quit XMonad"              quit)

  , ( "<XF86MonBrightnessUp>",     addName "Backlight Up"   backlight_up)
  , ( "<XF86MonBrightnessDown>",   addName "Backlight Down" backlight_down)
  , ( "S-<XF86MonBrightnessUp>",   addName "Backlight Max"  backlight_max)
  , ( "S-<XF86MonBrightnessDown>", addName "Backlight Min"  backlight_min)

  , ( "<XF86KbdBrightnessUp>",     addName "Keyboard Backlight Up"   kbd_backlight_up)
  , ( "<XF86KbdBrightnessDown>",   addName "Keyboard Backlight Down" kbd_backlight_down)
  , ( "S-<XF86KbdBrightnessUp>",   addName "Keyboard Backlight Max"  kbd_backlight_max)
  , ( "S-<XF86KbdBrightnessDown>", addName "Keyboard Backlight Min"  kbd_backlight_min)

  , ( "<XF86AudioMute>",          addName "Volume Mute" vol_mute)
  , ( "<XF86AudioLowerVolume>",   addName "Volume Down" vol_down)
  , ( "<XF86AudioRaiseVolume>",   addName "Volume Up"   vol_up)
  , ( "S-<XF86AudioLowerVolume>", addName "Volume Min"  vol_min)
  , ( "S-<XF86AudioRaiseVolume>", addName "Volume Max"  vol_max)
  ]

restart         = spawn "xmonad --restart"
rebuild_restart = spawn "~/git/xmonad-conf/build.sh -r"

quit = confirmPrompt hotPrompt "Quit XMonad" $ io (exitWith ExitSuccess)

backlight_up   = spawn "displayctl -n int up"
backlight_down = spawn "displayctl -n int down"
backlight_max  = spawn "displayctl -n int max"
backlight_min  = spawn "displayctl -n int min"

kbd_backlight_up   = spawn "displayctl -n kbd up"
kbd_backlight_down = spawn "displayctl -n kbd down"
kbd_backlight_max  = spawn "displayctl -n kbd max"
kbd_backlight_min  = spawn "displayctl -n kbd min"

vol_up   = spawn "audioctl -n vol up"
vol_down = spawn "audioctl -n vol down"
vol_mute = spawn "audioctl -n vol mute"
vol_max= spawn "audioctl -n vol max"
vol_min= spawn "audioctl -n vol min"
