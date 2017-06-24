{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable,
  TypeSynonymInstances, MultiParamTypeClasses #-}

----------------------------------------------------------------------------
-- |
-- Module       : Conf.Hooks.Fade
-- Copyright    : (c) maddy@na.ai
-- License      : MIT
--
-- Maintainer   : maddy@na.ai
-- Stability    : unstable
-- Portability  : unportable
--
----------------------------------------------------------------------------
module Conf.Hooks.Fade
  ( fade
  ) where

import Data.List (isPrefixOf)

import qualified XMonad

import XMonad (className)
import XMonad.Hooks.FadeWindows (isUnfocused, opacity, opaque)
import XMonad.Hooks.ManageHelpers (isDialog)

import XMonad ((-->), (<&&>), (=?))

fade =
  XMonad.composeAll
    [ opaque -- default to opaque
    , isUnfocused --> opacity 0.85
    , (className =? "Terminator") <&&> (isUnfocused) --> opacity 0.9 -- TODO: Don't hardcode
    , (className =? "URxvt") <&&> (isUnfocused) --> opacity 0.9 -- TODO: Don't hardcode
    , fmap ("Google" `isPrefixOf`) className --> opaque -- TODO: Don't hardcode
    , isDialog --> opaque
    ]
