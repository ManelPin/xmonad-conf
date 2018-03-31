{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses, NoMonomorphismRestriction, FlexibleContexts #-}

----------------------------------------------------------------------------
-- |
-- Module       : Conf.Layouts.Stacked
-- Copyright    : (c) maddy@na.ai
-- License      : MIT
--
-- Maintainer   : maddy@na.ai
-- Stability    : unstable
-- Portability  : unportable
--
----------------------------------------------------------------------------
module Conf.Layouts.Stacked
  ( stacked
  , name
  ) where

import Conf.Layouts.Internal (addTopBar, named)

import qualified Conf.Theme as Theme

import Conf.Theme.Gaps (gaps, spacing)

import qualified XMonad.Layout.Column as Column

import XMonad.Hooks.ManageDocks (avoidStruts)

name = "ST"

stacked
  = named name
  $ avoidStruts
  $ addTopBar
  $ gaps
  $ spacing
  $ Column.Column 1
