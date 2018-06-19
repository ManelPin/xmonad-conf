{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable,
  TypeSynonymInstances, MultiParamTypeClasses #-}

----------------------------------------------------------------------------
-- |
-- Module       : Conf.Bindings.Keys.Windows
-- Copyright    : (c) maddy@na.ai
-- License      : MIT
--
-- Maintainer   : maddy@na.ai
-- Stability    : unstable
-- Portability  : unportable
--
----------------------------------------------------------------------------
module Conf.Bindings.Keys.Windows
  ( windows
  ) where

import Conf.Bindings.Keys.Internal (subKeys, zipM, zipM', dirKeys, arrowKeys, dirs)

import qualified Conf.Layouts.DST as DST
import qualified Conf.Layouts.Tabs as Tabs

import Conf.Theme (hotPrompt)

import qualified XMonad
import qualified XMonad.Operations as Operations
import qualified XMonad.StackSet as StackSet
import qualified XMonad.Actions.WithAll as WithAll
import qualified XMonad.Layout.ComboP as ComboP
import qualified XMonad.Layout.SubLayouts as SubLayouts

import XMonad (sendMessage, screenWorkspace, whenJust, spawn)
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.ConditionalKeys (bindOn, XCond(LD))
import XMonad.Actions.GroupNavigation (nextMatch, Direction(History))
import XMonad.Actions.Navigation2D (windowGo, windowSwap, screenGo, windowToScreen, screenSwap)
import XMonad.Layout.WindowNavigation (Direction2D(L, R))
import XMonad.Prompt.ConfirmPrompt (confirmPrompt)
import XMonad.Util.NamedActions (addName)
import XMonad.Util.NamedScratchpad (scratchpadWorkspaceTag)

windows c = subKeys "Windows" c
  ( [ ("M-<Backspace>",   addName "Kill"                   kill1)
    , ("M-S-<Backspace>", addName "Kill all"               killAll)
    , ("M-'",             addName "Navigate tabs D"        navD)
    , ("M-;",             addName "Navigate tabs U"        navU)
    , ("C-'",             addName "Swap tab D"             swapD)
    , ("C-;",             addName "Swap tab U"             swapU)
    , ("M-S-/",           addName "Move window to NSP"   $ shiftToNSP)
    , ("M-S-;",           addName "Navigate window L"    $ windowGo L True)
    , ("M-S-'",           addName "Navigate window R"    $ windowGo R True)
    , ("M-C-S-m",         addName "Combo swap"             comboSwap)
    , ("M-<KP_End>",      addName "Select left screen"   $ selectScreen 1)
    , ("M-<KP_Down>",     addName "Select main screen"   $ selectScreen 0)
    , ("M-<KP_Insert>",   addName "Select bottom screen" $ selectScreen 2)
    , ("M-<period>",      addName "Select Prev Window"   $ nextMatch History (return True))

    , ("M-S-`", addName "Move cursor to active window" cursorToActiveWin)
    ]
  ++ zipM  "M-C-"   "Merge w/sublayout"        dirKeys   dirs mergeSub
  ++ zipM' "M-"     "Navigate window"          dirKeys   dirs windowGo        True
  ++ zipM' "C-S-"   "Swap window"              dirKeys   dirs windowSwap      True
  ++ zipM' "M-S-"   "Navigate screen"          dirKeys   dirs screenGo        True
  ++ zipM' "M-"     "Move window to screen"    arrowKeys dirs windowToScreen' True
  ++ zipM' "M-S-"   "Swap workspace to screen" arrowKeys dirs screenSwap      True
  )

killAll = confirmPrompt hotPrompt "kill all" $ WithAll.killAll

navD = bindOn LD
  [ (Tabs.name,  XMonad.windows     StackSet.focusDown)
  , (DST.name,   XMonad.windows     StackSet.focusDown)
  , ("",         SubLayouts.onGroup StackSet.focusDown')
  ]

navU = bindOn LD
  [ (Tabs.name,   XMonad.windows     StackSet.focusUp)
  , (DST.name,    XMonad.windows     StackSet.focusUp)
  , ("",          SubLayouts.onGroup StackSet.focusUp')
  ]

selectScreen sid = screenWorkspace sid >>= flip whenJust (Operations.windows . StackSet.view)

cursorToActiveWin
  -- TODO: create a mousectl script for stuff like this
  = spawn "xdotool getactivewindow mousemove --window '%1' 0 0 mousemove_relative --sync 1 1"

windowToScreen' d b = do
  windowToScreen d b
  windowGo d b
  return ()

swapD = XMonad.windows StackSet.swapDown
swapU = XMonad.windows StackSet.swapUp

shiftToNSP = XMonad.windows . StackSet.shift $ scratchpadWorkspaceTag

comboSwap = sendMessage $ ComboP.SwapWindow
mergeSub  = sendMessage . SubLayouts.pullGroup
