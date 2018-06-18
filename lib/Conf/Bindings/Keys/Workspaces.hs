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

import qualified XMonad.Actions.CycleWS as CycleWS

import qualified XMonad.Layout.IndependentScreens as IndependentScreens

import qualified XMonad.Util.NamedScratchpad as NamedScratchpad

import XMonad.Layout.IndependentScreens
        ( VirtualWorkspace
        )

import XMonad.Util.NamedActions
        ( addName
        )

workspaces c = subKeys "Workspaces & Projects" c
  ( [ ( "M-a",   addName "Toggle last workspace"    CycleWS.toggleWS)
    ]

  ++ [ ("M-" ++ show i, addName "View ws" $ windowView w)
     | (i, w) <- zip ([1..9] ++ [0]) (workspaces' c) ]

  ++ [ ("C-" ++ show i, addName "Move window to ws" $ windowMove w)
     | (i, w) <- zip ([1..9] ++ [0]) (workspaces' c) ]

  ++ [ ("C-M1-" ++ show i, addName "Move window to & view ws" $ windowMoveView w)
     | (i, w) <- zip ([1..9] ++ [0]) (workspaces' c) ]
  )

windowView w
  = do
    XMonad.windows $ IndependentScreens.onCurrentScreen StackSet.view w
    return ()

windowMove w
  = do
    XMonad.windows $ IndependentScreens.onCurrentScreen StackSet.shift w
    return ()

windowMoveView w
  = do
    windowMove w
    windowView w

workspaces' :: XMonad.XConfig l -> [VirtualWorkspace]
workspaces' = nub . map IndependentScreens.unmarshallW . filterWS . XMonad.workspaces
  where
    filterWS = filter (/= NamedScratchpad.scratchpadWorkspaceTag)
