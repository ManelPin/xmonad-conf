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

import Conf.Theme (hotPrompt)
import Conf.Bindings.Keys.Internal (subKeys)

import System.Exit (exitWith, ExitCode(ExitSuccess))

import XMonad (spawn, io)
import XMonad.Prompt.ConfirmPrompt (confirmPrompt)
import XMonad.Util.NamedActions (addName)

system c = subKeys "System" c
  [ ( "M-q",   addName "Restart XMonad"           xm_restart)
  , ( "M-C-q", addName "Rebuild & restart XMonad" xm_rebuild_restart)
  , ( "M-S-q", addName "Quit XMonad"              xm_quit)

  -- MBP Keyboard
  , ( "C-S-<XF86PowerOff>", addName "Lock"     sys_lock)
  , ( "M-<XF86PowerOff>",   addName "Shutdown" sys_shutdown)
  , ( "M-S-<XF86PowerOff>", addName "Reboot"   sys_reboot)
  ----

  -- Das Keyboard
  , ( "C-S-<XF86Sleep>", addName "Lock"     sys_lock)
  , ( "M-<XF86Sleep>",   addName "Shutdown" sys_shutdown)
  , ( "M-S-<XF86Sleep>", addName "Reboot"   sys_reboot)

  , ("M-<XF86AudioRaiseVolume>", addName "Backlight Up"   backlight_up)
  , ("M-<XF86AudioLowerVolume>", addName "Backlight Down" backlight_down)

  , ("M-S-<XF86AudioRaiseVolume>", addName "Keyboard Backlight Up"   kbd_backlight_up)
  , ("M-S-<XF86AudioLowerVolume>", addName "Keyboard Backlight Down" kbd_backlight_down)
  ----

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

  , ( "S-<XF86AudioLowerVolume>", addName "Volume Super Down" vol_sdown)
  , ( "S-<XF86AudioRaiseVolume>", addName "Volume Super Up"   vol_sup)


  , ( "<XF86AudioPlay>",   addName "Audio Play/Pause" audio_play)
  , ( "<XF86AudioPrev>",   addName "Audio Prev"       audio_prev)
  , ( "<XF86AudioNext>",   addName "Audio Next"       audio_next)
  , ( "S-<XF86AudioPlay>", addName "Audio Favorite"   audio_fav)
  ]

xm_restart         = spawn "xmonad --restart"
xm_rebuild_restart = spawn "$HOME/git/xmonad-conf/build.sh -r" -- TODO: Don't directly reference this path

xm_quit = confirmPrompt hotPrompt "Quit XMonad" $ io (exitWith ExitSuccess)

sys_lock     = spawn "system lock"
sys_reboot   = confirmPrompt hotPrompt "Reboot System"   $ spawn "reboot -p"
sys_shutdown = confirmPrompt hotPrompt "Shutdown System" $ spawn "shutdown -t now"

backlight_up   = spawn "displayctl -n int up"
backlight_down = spawn "displayctl -n int down"
backlight_max  = spawn "displayctl -n int max"
backlight_min  = spawn "displayctl -n int min"

kbd_backlight_up   = spawn "displayctl -n kbd up"
kbd_backlight_down = spawn "displayctl -n kbd down"
kbd_backlight_max  = spawn "displayctl -n kbd max"
kbd_backlight_min  = spawn "displayctl -n kbd min"

vol_up    = spawn "audioctl -n vol up"
vol_down  = spawn "audioctl -n vol down"
vol_mute  = spawn "audioctl -n vol mute"
vol_sup   = spawn "audioctl -n vol sup"
vol_sdown = spawn "audioctl -n vol sdown"

audio_play = spawn "audioctl player play-pause"
audio_prev = spawn "audioctl player previous"
audio_next = spawn "audioctl player next"
audio_fav  = spawn "audioctl player favorite"
