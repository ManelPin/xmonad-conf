{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable,
  TypeSynonymInstances, MultiParamTypeClasses #-}

----------------------------------------------------------------------------
-- |
-- Module       : Conf.Hooks.HandleEvent
-- Copyright    : (c) maddy@na.ai
-- License      : MIT
--
-- Maintainer   : maddy@na.ai
-- Stability    : unstable
-- Portability  : unportable
--
----------------------------------------------------------------------------
module Conf.Hooks.HandleEvent
  ( handleEvent
  ) where

import qualified XMonad

import qualified XMonad.Hooks.FadeWindows as FadeWindows
import qualified XMonad.Hooks.ManageDocks as ManageDocks

import qualified XMonad.Layout.Fullscreen as Fullscreen

import XMonad.Prompt (def)

import XMonad ((<+>))

handleEvent
  = ManageDocks.docksEventHook
  <+> FadeWindows.fadeWindowsEventHook
  <+> XMonad.handleEventHook def
  <+> Fullscreen.fullscreenEventHook
