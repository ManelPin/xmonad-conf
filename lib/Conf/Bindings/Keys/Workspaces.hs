{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable,
  TypeSynonymInstances, MultiParamTypeClasses #-}

----------------------------------------------------------------------------
-- |
-- Module       : Conf.Bindings.Keys.Workspaces
-- Copyright    : (c) maddy@na.ai
-- License      : MIT
--
-- Maintainer   : maddy@na.ai
-- Stability    : unstable
-- Portability  : unportable
--
----------------------------------------------------------------------------
module Conf.Bindings.Keys.Workspaces
  ( workspaces
  ) where

import Conf.Bindings.Keys.Internal
        ( subKeys
        )

import Data.List
        ( nub
        )

import qualified XMonad
import qualified XMonad.StackSet as StackSet
import qualified XMonad.Prompt as Prompt

import qualified XMonad.Actions.CycleWS as CycleWS

import qualified XMonad.Layout.IndependentScreens as IndependentScreens

import qualified XMonad.Util.NamedScratchpad as NamedScratchpad
import qualified XMonad.Util.WorkspaceCompare as WorkspaceCompare

import XMonad.Layout.IndependentScreens
        ( VirtualWorkspace
        )

import XMonad.Util.NamedActions
        ( addName
        )

workspaces c = subKeys "Workspaces & Projects" c
  ( [ ( "M-`",   addName "Next non-empty workspace" nextNonEmptyWS)
    , ( "M-S-`", addName "Prev non-empty workspace" prevNonEmptyWS)
    , ( "M-a",   addName "Toggle last workspace"    CycleWS.toggleWS)
    ]
  ++ [ ("M-" ++ show i, addName "View ws" $ XMonad.windows $ IndependentScreens.onCurrentScreen StackSet.view w)
     | (i, w) <- zip ([1..9] ++ [0]) (workspaces' c) ]
  ++ [ ("C-" ++ show i, addName "Move ws" $ XMonad.windows $ IndependentScreens.onCurrentScreen StackSet.shift w)
     | (i, w) <- zip ([1..9] ++ [0]) (workspaces' c) ]
  )

workspaces' :: XMonad.XConfig l -> [VirtualWorkspace]
workspaces' = nub . map IndependentScreens.unmarshallW . filterWS . XMonad.workspaces
  where
    filterWS = filter (/= NamedScratchpad.scratchpadWorkspaceTag)

nextNonEmptyWS =
  CycleWS.findWorkspace getSortByIndexNoSP Prompt.Next CycleWS.HiddenNonEmptyWS 1 >>= \t ->
    (XMonad.windows . StackSet.view $ t)

prevNonEmptyWS =
  CycleWS.findWorkspace getSortByIndexNoSP Prompt.Prev CycleWS.HiddenNonEmptyWS 1 >>= \t ->
    (XMonad.windows . StackSet.view $ t)

getSortByIndexNoSP =
  fmap (. NamedScratchpad.namedScratchpadFilterOutWorkspace) WorkspaceCompare.getSortByIndex
