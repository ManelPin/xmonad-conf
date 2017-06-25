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
  [ ( "M-q",   addName "Restart XMonad"           $ restart)
  , ( "M-C-q", addName "Rebuild & restart XMonad" $ rebuild_restart)
  , ( "M-S-q", addName "Quit XMonad"              $ quit)
  ]

restart         = spawn "xmonad --restart"
rebuild_restart = spawn "~/git/xmonad-conf/build.sh -r"

quit = confirmPrompt hotPrompt "Quit XMonad" $ io (exitWith ExitSuccess)
