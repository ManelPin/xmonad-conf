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

import Conf.Bindings.Keys.Internal (subKeys, zipM, wsKeys) --, modSym)

import Conf.Theme (warmPrompt)

import qualified XMonad
import qualified XMonad.StackSet as StackSet
import qualified XMonad.Prompt as Prompt

import qualified XMonad.Actions.CopyWindow as CopyWindow
import qualified XMonad.Actions.CycleWS as CycleWS
-- import qualified XMonad.Actions.CycleRecentWS as CycleRecentWS
import qualified XMonad.Actions.DynamicProjects as DynamicProjects

import qualified XMonad.Util.NamedScratchpad as NamedScratchpad
import qualified XMonad.Util.WorkspaceCompare as WorkspaceCompare

import XMonad.Actions.DynamicWorkspaces (withNthWorkspace)
import XMonad.Util.NamedActions (addName)

workspaces c = subKeys "Workspaces & Projects" c
  ( [ ( "M-w",   addName "Switch to Project"        switchProj)
    , ( "M-S-w", addName "Shift to Project"         shiftProj)
    , ( "M-`",   addName "Next non-empty workspace" nextNonEmptyWS)
    , ( "M-S-`", addName "Prev non-empty workspace" prevNonEmptyWS)
    , ( "M-a",   addName "Toggle last workspace"    toggleLast)
    -- , ( "M-o",   addName "Cycle recent workspaces"  cycleRecent)
  ]
  ++ zipM "M-"     "View      ws" wsKeys [0 ..] viewWs
  ++ zipM "C-"     "Move w to ws" wsKeys [0 ..] moveWinWs
  ++ zipM "M-S-C-" "Copy w to ws" wsKeys [0 ..] copyWinWs
  )

switchProj = DynamicProjects.switchProjectPrompt  warmPrompt
shiftProj  = DynamicProjects.shiftToProjectPrompt warmPrompt

nextNonEmptyWS =
  CycleWS.findWorkspace getSortByIndexNoSP Prompt.Next CycleWS.HiddenNonEmptyWS 1 >>= \t ->
    (XMonad.windows . StackSet.view $ t)

prevNonEmptyWS =
  CycleWS.findWorkspace getSortByIndexNoSP Prompt.Prev CycleWS.HiddenNonEmptyWS 1 >>= \t ->
    (XMonad.windows . StackSet.view $ t)

getSortByIndexNoSP =
  fmap (. NamedScratchpad.namedScratchpadFilterOutWorkspace) WorkspaceCompare.getSortByIndex

toggleLast = CycleWS.toggleWS' ["NSP"] -- Ignore NSP (Named Scratchpad)

-- cycleRecent = CycleRecentWS.cycleRecentWS [modSym] XMonad.xK_o XMonad.xK_i


viewWs    = withNthWorkspace StackSet.view
moveWinWs = withNthWorkspace StackSet.shift
copyWinWs = withNthWorkspace CopyWindow.copy
