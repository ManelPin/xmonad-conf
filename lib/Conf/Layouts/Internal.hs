{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable,
  TypeSynonymInstances, MultiParamTypeClasses #-}

----------------------------------------------------------------------------
-- |
-- Module       : Conf.Layouts.Internal
-- Copyright    : (c) maddy@na.ai
-- License      : MIT
--
-- Maintainer   : maddy@na.ai
-- Stability    : unstable
-- Portability  : unportable
--
----------------------------------------------------------------------------
module Conf.Layouts.Internal where

import qualified Conf.Theme as Theme

import Graphics.X11.Types (Window())
import XMonad.Layout.LayoutModifier (ModifiedLayout())
import XMonad.Layout.Decoration (Decoration())
import XMonad.Layout.Decoration (DefaultShrinker())
import XMonad.Layout.NoFrillsDecoration (NoFrillsDecoration())

import qualified XMonad.Layout.Tabbed as Tabbed
import qualified XMonad.Layout.Renamed as Renamed

import XMonad.Layout.NoFrillsDecoration (noFrillsDeco)

addTopBar :: l Window -> ModifiedLayout (Decoration NoFrillsDecoration DefaultShrinker) l Window
addTopBar = noFrillsDeco Tabbed.shrinkText Theme.topBar

named          n = Renamed.renamed [(Renamed.Replace n)]
suffixed       n = Renamed.renamed [(Renamed.AppendWords n)]
trimNamed    w n = Renamed.renamed [(Renamed.CutWordsLeft w), (Renamed.PrependWords n)]
trimSuffixed w n = Renamed.renamed [(Renamed.CutWordsRight w), (Renamed.AppendWords n)]

