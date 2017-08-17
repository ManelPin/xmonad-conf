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

import Conf.Theme (hotPrompt)

import qualified XMonad
import qualified XMonad.Operations as Operations
import qualified XMonad.StackSet as StackSet
import qualified XMonad.Actions.WithAll as WithAll
import qualified XMonad.Layout.ComboP as ComboP
import qualified XMonad.Layout.SubLayouts as SubLayouts

import XMonad (sendMessage, screenWorkspace, whenJust)
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.ConditionalKeys (bindOn, XCond(LD))
import XMonad.Actions.Navigation2D (windowGo, windowSwap, screenGo, windowToScreen, screenSwap)
import XMonad.Prompt.ConfirmPrompt (confirmPrompt)
import XMonad.Util.NamedActions (addName)

windows c = subKeys "Windows" c
  ( [ ("M-<Backspace>",   addName "Kill"            kill1)
    , ("M-S-<Backspace>", addName "Kill all"        killAll)
    , ("M-'",             addName "Navigate tabs D" navD)
    , ("M-;",             addName "Navigate tabs U" navU)
    , ("C-'",             addName "Swap tab D"      swapD)
    , ("C-;",             addName "Swap tab U"      swapU)
    , ("M-C-S-m",         addName "Combo swap"      comboSwap)

    , ("M-<KP_End>",      addName "Select left screen"   $ selectScreen 1)
    , ("M-<KP_Down>",     addName "Select main screen"   $ selectScreen 0)
    , ("M-<KP_Insert>",   addName "Select bottom screen" $ selectScreen 2)
    ]
  ++ zipM  "M-C-"   "Merge w/sublayout"        dirKeys   dirs mergeSub
  ++ zipM' "M-"     "Navigate window"          dirKeys   dirs windowGo       True
  ++ zipM' "C-"     "Move window"              dirKeys   dirs windowSwap     True
  ++ zipM' "M-S-"   "Navigate screen"          dirKeys   dirs screenGo       True
  ++ zipM' "M-S-C-" "Move window to screen"    dirKeys   dirs windowToScreen True
  ++ zipM' "M-"     "Swap workspace to screen" arrowKeys dirs screenSwap     True
  )

killAll = confirmPrompt hotPrompt "kill all" $ WithAll.killAll

navD = bindOn LD
  [ ("T",   XMonad.windows     StackSet.focusDown)
  , ("DST", XMonad.windows     StackSet.focusDown)
  , ("",    SubLayouts.onGroup StackSet.focusDown')
  ]

navU = bindOn LD
  [ ("T",    XMonad.windows     StackSet.focusUp)
  , ("DS T", XMonad.windows     StackSet.focusUp)
  , ("",     SubLayouts.onGroup StackSet.focusUp')
  ]

selectScreen sid = screenWorkspace sid >>= flip whenJust (Operations.windows . StackSet.view)

swapD = XMonad.windows StackSet.swapDown
swapU = XMonad.windows StackSet.swapUp

comboSwap = sendMessage $ ComboP.SwapWindow
mergeSub  = sendMessage . SubLayouts.pullGroup
