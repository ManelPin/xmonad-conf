{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable,
  TypeSynonymInstances, MultiParamTypeClasses #-}

----------------------------------------------------------------------------
-- |
-- Module       : Conf.Bindings.Keys.Resize
-- Copyright    : (c) maddy@na.ai
-- License      : MIT
--
-- Maintainer   : maddy@na.ai
-- Stability    : unstable
-- Portability  : unportable
--
----------------------------------------------------------------------------
module Conf.Bindings.Keys.Resize
  ( resize
  ) where

import Conf.Bindings.Keys.Internal (subKeys, tryMsgR)

import XMonad (Resize(Shrink, Expand))
import XMonad.Layout.ResizableTile (MirrorResize(MirrorShrink, MirrorExpand))
import XMonad.Layout.BinarySpacePartition (ResizeDirectional(ExpandTowards, ShrinkFrom))
import XMonad.Layout.WindowNavigation (Direction2D(D, U, L, R))

import XMonad.Util.NamedActions (addName)

resize c = subKeys "Resize" c
  [ ( "M-[",     addName "Expand (L on BSP)" $ tryMsgR (ExpandTowards L) (Shrink))
  , ( "M-]",     addName "Expand (R on BSP)" $ tryMsgR (ExpandTowards R) (Expand))
  , ( "M-S-[",   addName "Expand (U on BSP)" $ tryMsgR (ExpandTowards U) (MirrorShrink))
  , ( "M-S-]",   addName "Expand (D on BSP)" $ tryMsgR (ExpandTowards D) (MirrorExpand))
  , ( "M-C-[",   addName "Shrink (L on BSP)" $ tryMsgR (ShrinkFrom R)    (Shrink))
  , ( "M-C-]",   addName "Shrink (R on BSP)" $ tryMsgR (ShrinkFrom L)    (Expand))
  , ( "M-C-S-[", addName "Shrink (U on BSP)" $ tryMsgR (ShrinkFrom D)    (MirrorShrink))
  , ( "M-C-S-]", addName "Shrink (D on BSP)" $ tryMsgR (ShrinkFrom U)    (MirrorExpand))
  ]
