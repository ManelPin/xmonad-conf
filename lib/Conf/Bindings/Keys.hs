{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable,
  TypeSynonymInstances, MultiParamTypeClasses #-}

----------------------------------------------------------------------------
-- |
-- Module       : Conf.Bindings.Keys
-- Copyright    : (c) maddy@na.ai
-- License      : MIT
--
-- Maintainer   : maddy@na.ai
-- Stability    : unstable
-- Portability  : unportable
--
----------------------------------------------------------------------------
module Conf.Bindings.Keys
  ( modMask
  , keys
  ) where

import qualified Conf.Applications as Applications
import qualified Conf.Theme as Theme

import qualified Data.Map as Map
import qualified System.Exit as Exit

import qualified XMonad
import qualified XMonad.StackSet as StackSet

import qualified XMonad.Actions.CycleWS as CycleWS
import qualified XMonad.Actions.ConditionalKeys as ConditionalKeys
import qualified XMonad.Actions.CopyWindow as CopyWindow
import qualified XMonad.Actions.DynamicProjects as DynamicProjects
import qualified XMonad.Actions.DynamicWorkspaces as DynamicWorkspaces
import qualified XMonad.Actions.MessageFeedback as MessageFeedback
import qualified XMonad.Actions.Navigation2D as Navigation2D
import qualified XMonad.Actions.WithAll as WithAll

import qualified XMonad.Layout.BinarySpacePartition as BinarySpacePartition
import qualified XMonad.Layout.ComboP as ComboP
import qualified XMonad.Layout.MultiToggle as MultiToggle
import qualified XMonad.Layout.MultiToggle.Instances as MultiToggle.Instances
import qualified XMonad.Layout.Reflect as Reflect
import qualified XMonad.Layout.ResizableTile as ResizableTile
import qualified XMonad.Layout.SubLayouts as SubLayouts
import qualified XMonad.Layout.WindowNavigation as WindowNavigation

import qualified XMonad.Prompt as Prompt
import qualified XMonad.Prompt.ConfirmPrompt as ConfirmPrompt

import qualified XMonad.Util.NamedActions as NamedActions
import qualified XMonad.Util.EZConfig as EZConfig
import qualified XMonad.Util.NamedScratchpad as NamedScratchpad
import qualified XMonad.Util.Paste as Paste
import qualified XMonad.Util.WorkspaceCompare as WorkspaceCompare

import XMonad.Util.NamedActions ((^++^))

modMask = XMonad.mod4Mask

keys c
  =    system    c
  ^++^ launchers c
  ^++^ windows   c
  ^++^ layout    c
  ^++^ resize    c

wsKeys = map show $ [1 .. 9] ++ [0]

nextNonEmptyWS =
  CycleWS.findWorkspace getSortByIndexNoSP Prompt.Next CycleWS.HiddenNonEmptyWS 1 >>= \t ->
    (XMonad.windows . StackSet.view $ t)

prevNonEmptyWS =
  CycleWS.findWorkspace getSortByIndexNoSP Prompt.Prev CycleWS.HiddenNonEmptyWS 1 >>= \t ->
    (XMonad.windows . StackSet.view $ t)

getSortByIndexNoSP =
  fmap (. NamedScratchpad.namedScratchpadFilterOutWorkspace) WorkspaceCompare.getSortByIndex

dirKeys = ["j", "k", "h", "l"]
arrowKeys = ["<D>", "<U>", "<L>", "<R>"]
dirs = [WindowNavigation.D, WindowNavigation.U, WindowNavigation.L, WindowNavigation.R]
zipM m nm ks as f = zipWith (\k d -> (m ++ k, NamedActions.addName nm $ f d)) ks as
zipM' m nm ks as f b = zipWith (\k d -> (m ++ k, NamedActions.addName nm $ f d b)) ks as

-- try sending one message, fallback if unreceived, then refresh
tryMsgR x y = sequence_ [(MessageFeedback.tryMessage_ x y), XMonad.refresh]

toggleFloat w =
  XMonad.windows
    (\s ->
       if Map.member w (StackSet.floating s)
         then StackSet.sink w s
         else (StackSet.float w (StackSet.RationalRect (1 / 3) (1 / 4) (1 / 2) (4 / 5)) s))

subKeys str conf ks = NamedActions.subtitle str : EZConfig.mkNamedKeymap conf ks

-- system c = NamedActions.subtitle "System" : EZConfig.mkNamedKeymap c
system c = subKeys "System" c
  [ ("M-q", NamedActions.addName "Restart XMonad" $ XMonad.spawn "xmonad --restart")
  , ( "M-C-q"
   , NamedActions.addName "Rebuild & restart XMonad" $
     XMonad.spawn "xmonad --recompile && xmonad --restart")
  , ( "M-S-q"
   , NamedActions.addName "Quit XMonad" $
     ConfirmPrompt.confirmPrompt Theme.hotPrompt "Quit XMonad" $
     XMonad.io (Exit.exitWith Exit.ExitSuccess))
  ]

launchers c = subKeys "Launchers" c
  [ ("M-<Space>", NamedActions.addName "Launcher" $ XMonad.spawn Applications.launcher)
  , ("M-<Return>", NamedActions.addName "Terminal" $ XMonad.spawn Applications.terminal)
  , ("M-\\", NamedActions.addName "Browser" $ XMonad.spawn Applications.browser)
  , ("M-s s", NamedActions.addName "Cancel submap" $ return ())
  , ("M-s M-s", NamedActions.addName "Cancel submap" $ return ())
  ]


windows c = subKeys "Windows" c
  ([ ("M-<Backspace>", NamedActions.addName "Kill" CopyWindow.kill1)
  , ( "M-S-<Backspace>"
    , NamedActions.addName "Kill all" $
      ConfirmPrompt.confirmPrompt Theme.hotPrompt "kill all" $ WithAll.killAll)
  , ( "M-'"
    , NamedActions.addName "Navigate tabs D" $
      ConditionalKeys.bindOn
        ConditionalKeys.LD
        [ ("Tabs", XMonad.windows StackSet.focusDown)
        , ("", SubLayouts.onGroup StackSet.focusDown')
        ])
  , ( "M-;"
    , NamedActions.addName "Navigate tabs U" $
      ConditionalKeys.bindOn
        ConditionalKeys.LD
        [ ("Tabs", XMonad.windows StackSet.focusUp)
        , ("", SubLayouts.onGroup StackSet.focusUp')
        ])
  , ("C-'", NamedActions.addName "Swap tab D" $ XMonad.windows StackSet.swapDown)
  , ("C-;", NamedActions.addName "Swap tab U" $ XMonad.windows StackSet.swapUp)
  , ("M-C-S-m", NamedActions.addName "Combo swap" $ XMonad.sendMessage $ ComboP.SwapWindow)
  ] ++
  zipM' "M-" "Navigate window" dirKeys dirs Navigation2D.windowGo True
  -- ++ zipM' "M-S-"               "Move window"                               dirKeys dirs windowSwap True
  -- TODO: following may necessitate use of a "passthrough" binding that can send C- values to focused w
   ++
  zipM' "C-" "Move window" dirKeys dirs Navigation2D.windowSwap True ++
  zipM "M-C-" "Merge w/sublayout" dirKeys dirs (XMonad.sendMessage . SubLayouts.pullGroup) ++
  zipM' "M-" "Navigate screen" arrowKeys dirs Navigation2D.screenGo True
  -- ++ zipM' "M-S-"             "Move window to screen"                     arrowKeys dirs windowToScreen True
   ++
  zipM' "M-C-" "Move window to screen" arrowKeys dirs Navigation2D.windowToScreen True ++
  zipM' "M-S-" "Swap workspace to screen" arrowKeys dirs Navigation2D.screenSwap True)

workspaces c = subKeys "Workspaces & Projects" c
  ([ ( "M-w"
    , NamedActions.addName "Switch to Project" $
      DynamicProjects.switchProjectPrompt Theme.warmPrompt)
  , ( "M-S-w"
    , NamedActions.addName "Shift to Project" $
      DynamicProjects.shiftToProjectPrompt Theme.warmPrompt)
  , ("M-<Escape>", NamedActions.addName "Next non-empty workspace" $ nextNonEmptyWS)
  , ("M-S-<Escape>", NamedActions.addName "Prev non-empty workspace" $ prevNonEmptyWS)
  , ("M-`", NamedActions.addName "Next non-empty workspace" $ nextNonEmptyWS)
  , ("M-S-`", NamedActions.addName "Prev non-empty workspace" $ prevNonEmptyWS)
  , ("M-a", NamedActions.addName "Toggle last workspace" $ CycleWS.toggleWS' ["NSP"])
  ] ++
  zipM
    "M-"
    "View      ws"
    wsKeys
    [0 ..]
    (DynamicWorkspaces.withNthWorkspace StackSet.greedyView) ++
  zipM "C-" "Move w to ws" wsKeys [0 ..] (DynamicWorkspaces.withNthWorkspace StackSet.shift) ++
  zipM
    "M-S-C-"
    "Copy w to ws"
    wsKeys
    [0 ..]
    (DynamicWorkspaces.withNthWorkspace CopyWindow.copy)
  )

layout c = subKeys "Layout Management" c
  [ ( "M-<Tab>", NamedActions.addName "Cycle all layouts" $ XMonad.sendMessage XMonad.NextLayout)
  , ("M-C-<Tab>", NamedActions.addName "Cycle sublayout" $ SubLayouts.toSubl XMonad.NextLayout)
  , ( "M-S-<Tab>", NamedActions.addName "Reset layout" $ XMonad.setLayout $ XMonad.layoutHook c)
  , ("M-y", NamedActions.addName "Float tiled w" $ XMonad.withFocused toggleFloat)
  , ("M-S-y", NamedActions.addName "Tile all floating w" $ WithAll.sinkAll)
  , ( "M-,", NamedActions.addName "Decrease master windows" $ XMonad.sendMessage (XMonad.IncMasterN (-1)))
  , ( "M-.", NamedActions.addName "Increase master windows" $ XMonad.sendMessage (XMonad.IncMasterN 1))
  , ( "M-r", NamedActions.addName "Reflect/Rotate" $ tryMsgR (BinarySpacePartition.Rotate) (MultiToggle.Toggle Reflect.REFLECTX))
  , ( "M-S-r", NamedActions.addName "Force Reflect (even on BSP)" $ XMonad.sendMessage (MultiToggle.Toggle Reflect.REFLECTX))
  , ( "M-f", NamedActions.addName "Fullscreen" $ sequence_
      [ (XMonad.withFocused $ XMonad.windows . StackSet.sink)
      , (XMonad.sendMessage $ MultiToggle.Toggle MultiToggle.Instances.FULL)
      ])
  , ( "M-S-f", NamedActions.addName "Fake fullscreen" $ sequence_
      [ (Paste.sendKey Paste.noModMask XMonad.xK_F11)
      , (tryMsgR (BinarySpacePartition.ExpandTowards WindowNavigation.L) (XMonad.Shrink))
      , (tryMsgR (BinarySpacePartition.ExpandTowards WindowNavigation.R) (XMonad.Expand))
      ])
  , ( "C-S-h", NamedActions.addName "Ctrl-h passthrough" $ Paste.sendKey XMonad.controlMask XMonad.xK_h)
  , ( "C-S-j", NamedActions.addName "Ctrl-j passthrough" $ Paste.sendKey XMonad.controlMask XMonad.xK_j)
  , ( "C-S-k", NamedActions.addName "Ctrl-k passthrough" $ Paste.sendKey XMonad.controlMask XMonad.xK_k)
  , ( "C-S-l", NamedActions.addName "Ctrl-l passthrough" $ Paste.sendKey XMonad.controlMask XMonad.xK_l)
  ]

resize c = subKeys "Resize" c
  [ ( "M-[", NamedActions.addName "Expand (L on BSP)" $ tryMsgR (BinarySpacePartition.ExpandTowards WindowNavigation.L) (XMonad.Shrink))
  , ( "M-]", NamedActions.addName "Expand (R on BSP)" $ tryMsgR (BinarySpacePartition.ExpandTowards WindowNavigation.R) (XMonad.Expand))
  , ( "M-S-[", NamedActions.addName "Expand (U on BSP)" $ tryMsgR (BinarySpacePartition.ExpandTowards WindowNavigation.U) (ResizableTile.MirrorShrink))
  , ( "M-S-]", NamedActions.addName "Expand (D on BSP)" $ tryMsgR (BinarySpacePartition.ExpandTowards WindowNavigation.D) (ResizableTile.MirrorExpand))
  , ( "M-C-[", NamedActions.addName "Shrink (L on BSP)" $ tryMsgR (BinarySpacePartition.ShrinkFrom WindowNavigation.R) (XMonad.Shrink))
  , ( "M-C-]", NamedActions.addName "Shrink (R on BSP)" $ tryMsgR (BinarySpacePartition.ShrinkFrom WindowNavigation.L) (XMonad.Expand))
  , ( "M-C-S-[", NamedActions.addName "Shrink (U on BSP)" $ tryMsgR (BinarySpacePartition.ShrinkFrom WindowNavigation.D) (ResizableTile.MirrorShrink))
  , ( "M-C-S-]", NamedActions.addName "Shrink (D on BSP)" $ tryMsgR (BinarySpacePartition.ShrinkFrom WindowNavigation.U) (ResizableTile.MirrorExpand))
  ]

