{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable,
  TypeSynonymInstances, MultiParamTypeClasses #-}

----------------------------------------------------------------------------
-- |
-- Module       : Conf.XMonad
-- Copyright    : (c) maddy@na.ai
-- License      : MIT
--
-- Maintainer   : maddy@na.ai
-- Stability    : unstable
-- Portability  : unportable
--
----------------------------------------------------------------------------
module Conf.XMonad
  ( xmonad
  ) where

import qualified Conf.Applications as Applications
import qualified Conf.Workspaces as Workspaces

import qualified Conf.Bindings.Keys as Bindings.Keys
import qualified Conf.Bindings.Mouse as Bindings.Mouse

import qualified Conf.Hooks.HandleEvent as Hooks.HandleEvent
import qualified Conf.Hooks.Manage as Hooks.Manage
import qualified Conf.Hooks.Layout as Hooks.Layout
import qualified Conf.Hooks.Log as Hooks.Log
import qualified Conf.Hooks.Startup as Hooks.Startup

import qualified Conf.Theme as Theme
import qualified Conf.Theme.Colors as Theme.Colors
import qualified Conf.Theme.Sizes as Theme.Sizes

import qualified XMonad
import qualified XMonad.Prompt as Prompt

xmonad p =
  Prompt.def
  { XMonad.borderWidth        = Theme.Sizes.border
  , XMonad.clickJustFocuses   = Theme.clickJustFocuses
  , XMonad.focusFollowsMouse  = Theme.focusFollowsMouse
  , XMonad.normalBorderColor  = Theme.Colors.normalBorder
  , XMonad.focusedBorderColor = Theme.Colors.focusedBorder
  , XMonad.manageHook         = Hooks.Manage.manage
  , XMonad.handleEventHook    = Hooks.HandleEvent.handleEvent
  , XMonad.layoutHook         = Hooks.Layout.layout
  , XMonad.logHook            = Hooks.Log.log p
  , XMonad.modMask            = Bindings.Keys.modMask
  , XMonad.mouseBindings      = Bindings.Mouse.mouse
  , XMonad.startupHook        = Hooks.Startup.startup
  , XMonad.terminal           = Applications.terminal
  , XMonad.workspaces         = Workspaces.workspaces
  }
