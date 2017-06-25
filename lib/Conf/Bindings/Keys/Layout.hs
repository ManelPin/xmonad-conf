{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable,
  TypeSynonymInstances, MultiParamTypeClasses #-}

----------------------------------------------------------------------------
-- |
-- Module       : Conf.Bindings.Keys.Layout
-- Copyright    : (c) maddy@na.ai
-- License      : MIT
--
-- Maintainer   : maddy@na.ai
-- Stability    : unstable
-- Portability  : unportable
--
----------------------------------------------------------------------------
module Conf.Bindings.Keys.Layout
  ( layout
  ) where

import Conf.Bindings.Keys.Internal (subKeys, tryMsgR)

import qualified Data.Map as Map

import qualified XMonad
import qualified XMonad.StackSet as StackSet

import qualified XMonad.Actions.WithAll as WithAll

import qualified XMonad.Layout.BinarySpacePartition as BinarySpacePartition
import qualified XMonad.Layout.MultiToggle as MultiToggle
import qualified XMonad.Layout.MultiToggle.Instances as MultiToggle.Instances
import qualified XMonad.Layout.Reflect as Reflect
import qualified XMonad.Layout.SubLayouts as SubLayouts
import qualified XMonad.Layout.WindowNavigation as WindowNavigation

import qualified XMonad.Util.Paste as Paste

import XMonad.Util.NamedActions (addName)

layout c = subKeys "Layout Management" c
  [ ( "M-S-<Tab>", addName "Reset layout"                $ reset c)
  , ( "M-<Tab>",   addName "Cycle all layouts"           cycleAll)
  , ( "M-C-<Tab>", addName "Cycle sublayout"             cycleSub)
  , ( "M-y",       addName "Float tiled w"               floatTiled)
  , ( "M-S-y",     addName "Tile all floating w"         tileFloating)
  , ( "M-,",       addName "Decrease master windows"     decMaster)
  , ( "M-.",       addName "Increase master windows"     incMaster)
  , ( "M-r",       addName "Reflect/Rotate"              rotate)
  , ( "M-S-r",     addName "Force Reflect (even on BSP)" reflect)
  , ( "M-f",       addName "Fullscreen"                  fullscreen)
  , ( "M-S-f",     addName "Fake fullscreen"             fakeFullscreen)
  , ( "C-S-h",     addName "Ctrl-h passthrough"          hPass)
  , ( "C-S-j",     addName "Ctrl-j passthrough"          jPass)
  , ( "C-S-k",     addName "Ctrl-k passthrough"          kPass)
  , ( "C-S-l",     addName "Ctrl-l passthrough"          lPass)
  ]

cycleAll     = XMonad.sendMessage XMonad.NextLayout
cycleSub     = SubLayouts.toSubl  XMonad.NextLayout
reset c      = XMonad.setLayout $ XMonad.layoutHook c
floatTiled   = XMonad.withFocused toggleFloat
tileFloating = WithAll.sinkAll
decMaster    = XMonad.sendMessage (XMonad.IncMasterN (-1))
incMaster    = XMonad.sendMessage (XMonad.IncMasterN 1)
rotate       = tryMsgR (BinarySpacePartition.Rotate) (MultiToggle.Toggle Reflect.REFLECTX)
reflect      = XMonad.sendMessage (MultiToggle.Toggle Reflect.REFLECTX)

fullscreen = sequence_
  [ (XMonad.withFocused $ XMonad.windows . StackSet.sink)
  , (XMonad.sendMessage $ MultiToggle.Toggle MultiToggle.Instances.FULL)
  ]

fakeFullscreen = sequence_
  [ (Paste.sendKey Paste.noModMask XMonad.xK_F11)
  , (tryMsgR (BinarySpacePartition.ExpandTowards WindowNavigation.L) (XMonad.Shrink))
  , (tryMsgR (BinarySpacePartition.ExpandTowards WindowNavigation.R) (XMonad.Expand))
  ]

hPass = Paste.sendKey XMonad.controlMask XMonad.xK_h
jPass = Paste.sendKey XMonad.controlMask XMonad.xK_j
kPass = Paste.sendKey XMonad.controlMask XMonad.xK_k
lPass = Paste.sendKey XMonad.controlMask XMonad.xK_l

toggleFloat w =
  XMonad.windows
    (\s ->
       if Map.member w (StackSet.floating s)
         then StackSet.sink w s
         else (StackSet.float w (StackSet.RationalRect (1 / 3) (1 / 4) (1 / 2) (4 / 5)) s))
