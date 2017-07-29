{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable,
  TypeSynonymInstances, MultiParamTypeClasses #-}

----------------------------------------------------------------------------
-- |
-- Module       : Conf.Hooks.StatusBars
-- Copyright    : (c) maddy@na.ai
-- License      : MIT
--
-- Maintainer   : maddy@na.ai
-- Stability    : unstable
-- Portability  : unportable
--
----------------------------------------------------------------------------
module Conf.Hooks.StatusBars
  ( startupHook
  , eventHook
  ) where

import qualified Conf.Applications as Apps

import qualified XMonad
import qualified XMonad.Hooks.DynamicBars as DynamicBars
import qualified XMonad.Util.Run as Run

startupHook = DynamicBars.dynStatusBarStartup   statusBarStart statusBarClean
eventHook   = DynamicBars.dynStatusBarEventHook statusBarStart statusBarClean

statusBarStart :: DynamicBars.DynamicStatusBar
statusBarStart (XMonad.S id) = Run.spawnPipe $
  Apps.statusBar ++ " -x " ++ show id ++ " " ++ Apps.statusBarConf

statusBarClean :: DynamicBars.DynamicStatusBarCleanup
statusBarClean = return ()
  -- do
  -- Run.unsafeSpawn $ "killall " ++ Apps.statusBar
  -- return ()
