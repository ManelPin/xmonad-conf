{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable,
  TypeSynonymInstances, MultiParamTypeClasses #-}

----------------------------------------------------------------------------
-- |
-- Module       : Conf.Bindings.Show
-- Copyright    : (c) maddy@na.ai
-- License      : MIT
--
-- Maintainer   : maddy@na.ai
-- Stability    : unstable
-- Portability  : unportable
--
----------------------------------------------------------------------------
module Conf.Bindings.Show
  ( show
  ) where

import Prelude hiding (show)

import qualified System.IO as IO

import qualified XMonad

import qualified XMonad.Util.NamedActions as NamedActions
import qualified XMonad.Util.Run as Run

show :: [((XMonad.KeyMask, XMonad.KeySym), NamedActions.NamedAction)] -> NamedActions.NamedAction
show x =
  NamedActions.addName "Show Keybindings" $
  XMonad.io $ do
    h <- Run.spawnPipe "yad --text-info"
    Run.hPutStr h (unlines $ NamedActions.showKm x)
    IO.hClose h
    return ()

