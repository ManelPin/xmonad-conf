{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable,
  TypeSynonymInstances, MultiParamTypeClasses #-}

import qualified Conf.Applications
import qualified Conf.Projects
import qualified Conf.Workspaces
import qualified Conf.XMonad

import qualified Data.List as List
import qualified Data.Map as Map
import qualified System.Exit as Exit
import qualified System.IO as IO

import qualified XMonad as XMonad
import qualified XMonad.StackSet as StackSet

import qualified XMonad.Actions.ConditionalKeys as ConditionalKeys
import qualified XMonad.Actions.ConstrainedResize
       as ConstrainedResize
import qualified XMonad.Actions.CopyWindow as CopyWindow
import qualified XMonad.Actions.CycleWS as CycleWS

import qualified XMonad.Actions.DynamicProjects as DynamicProjects
import qualified XMonad.Actions.DynamicWorkspaces
       as DynamicWorkspaces
import qualified XMonad.Actions.FloatSnap as FloatSnap
import qualified XMonad.Actions.MessageFeedback as MessageFeedback
import qualified XMonad.Actions.Navigation2D as Navigation2D
import qualified XMonad.Actions.SpawnOn as SpawnOn
import qualified XMonad.Actions.WithAll as WithAll

import qualified XMonad.Hooks.DynamicLog as DynamicLog
import qualified XMonad.Hooks.EwmhDesktops as EwmhDesktops
import qualified XMonad.Hooks.FadeWindows as FadeWindows
import qualified XMonad.Hooks.InsertPosition as InsertPosition
import qualified XMonad.Hooks.ManageDocks as ManageDocks
import qualified XMonad.Hooks.ManageHelpers as ManageHelpers
import qualified XMonad.Hooks.UrgencyHook as UrgencyHook

import qualified XMonad.Layout.Accordion as Accordion
import qualified XMonad.Layout.BinarySpacePartition
       as BinarySpacePartition
import qualified XMonad.Layout.ComboP as ComboP
import qualified XMonad.Layout.DecorationMadness

import qualified XMonad.Layout.Fullscreen as Fullscreen
import qualified XMonad.Layout.Gaps as Gaps
import qualified XMonad.Layout.Hidden as Hidden
import qualified XMonad.Layout.LayoutCombinators
       as LayoutCombinators
import qualified XMonad.Layout.MultiToggle as MultiToggle
import qualified XMonad.Layout.MultiToggle.Instances
       as MultiToggle.Instances
import qualified XMonad.Layout.NoFrillsDecoration
       as NoFrillsDecoration
import qualified XMonad.Layout.PerScreen as PerScreen
import qualified XMonad.Layout.Reflect as Reflect
import qualified XMonad.Layout.Renamed as Renamed
import qualified XMonad.Layout.ResizableTile as ResizableTile
import qualified XMonad.Layout.ShowWName as ShowWName
import qualified XMonad.Layout.Simplest as Simplest
import qualified XMonad.Layout.Spacing as Spacing
import qualified XMonad.Layout.SubLayouts as SubLayouts
import qualified XMonad.Layout.Tabbed as Tabbed
import qualified XMonad.Layout.ThreeColumns as ThreeColumns
import qualified XMonad.Layout.WindowNavigation as WindowNavigation

import qualified XMonad.Prompt as Prompt
import qualified XMonad.Prompt.ConfirmPrompt as ConfirmPrompt

import qualified XMonad.Util.Cursor as Cursor
import qualified XMonad.Util.EZConfig as EZConfig
import qualified XMonad.Util.NamedActions as NamedActions
import qualified XMonad.Util.NamedScratchpad as NamedScratchpad
import qualified XMonad.Util.NamedWindows as NamedWindows
import qualified XMonad.Util.Paste as Paste
import qualified XMonad.Util.Run as Run
import qualified XMonad.Util.WorkspaceCompare as WorkspaceCompare

main = do
  xmobarProc <- Run.spawnPipe Conf.Applications.statusBar
  XMonad.xmonad $
    Conf.Projects.dynamicProjects $
    Navigation2D.withNavigation2DConfig myNav2DConf $
    EwmhDesktops.ewmh $
    -- NamedActions.addDescrKeys' ((myModMask, XMonad.xK_F1), showKeybindings) myKeys $ myConfig xmobarProc
    NamedActions.addDescrKeys' ((myModMask, XMonad.xK_F1), showKeybindings) myKeys $
    Conf.XMonad.conf xmobarProc

------------------------------------------------------------------------}}}
-- Layouts                                                              {{{
myNav2DConf =
  Prompt.def
  { Navigation2D.defaultTiledNavigation = Navigation2D.centerNavigation
  , Navigation2D.floatNavigation = Navigation2D.centerNavigation
  , Navigation2D.screenNavigation = Navigation2D.lineNavigation
  , Navigation2D.layoutNavigation =
      [ ("Full", Navigation2D.centerNavigation)
    -- line/center same results   ,("Simple Tabs", lineNavigation)
    --                            ,("Simple Tabs", centerNavigation)
      ]
  , Navigation2D.unmappedWindowRect =
      [ ("Full", Navigation2D.singleWindowRect)
    -- works but breaks tab deco  ,("Simple Tabs", singleWindowRect)
    -- doesn't work but deco ok   ,("Simple Tabs", fullScreenRect)
      ]
  }

-- data FULLBAR =
--   FULLBAR
--   deriving (Read, Show, Eq, XMonad.Typeable)
--
-- instance MultiToggle.Transformer FULLBAR XMonad.Window where
--   transform FULLBAR x k = k barFull (\_ -> x)
--
-- barFull = ManageDocks.avoidStruts $ Simplest.Simplest
--
-- -- cf http://xmonad.org/xmonad-docs/xmonad-contrib/src/XMonad-Config-Droundy.html
-- myLayoutHook =
--   showWorkspaceName
--              -- $ onWorkspace wsFLOAT floatWorkSpace
--    $
--   Fullscreen.fullscreenFloat -- fixes floating windows going full screen, while retaining "bounded" fullscreen
--    $
--   fullScreenToggle $ fullBarToggle $ mirrorToggle $ reflectToggle $ flex LayoutCombinators.||| tabs
--     -- floatWorkSpace      = simplestFloat
--   where
--     fullBarToggle = MultiToggle.mkToggle (MultiToggle.single FULLBAR)
--     fullScreenToggle = MultiToggle.mkToggle (MultiToggle.single MultiToggle.Instances.FULL)
--     mirrorToggle = MultiToggle.mkToggle (MultiToggle.single MultiToggle.Instances.MIRROR)
--     reflectToggle = MultiToggle.mkToggle (MultiToggle.single Reflect.REFLECTX)
--     smallMonResWidth = 1920
--     showWorkspaceName = ShowWName.showWName' myShowWNameTheme
--     named n = Renamed.renamed [(Renamed.Replace n)]
--     trimNamed w n = Renamed.renamed [(Renamed.CutWordsLeft w), (Renamed.PrependWords n)]
--     suffixed n = Renamed.renamed [(Renamed.AppendWords n)]
--     trimSuffixed w n = Renamed.renamed [(Renamed.CutWordsRight w), (Renamed.AppendWords n)]
--     addTopBar = NoFrillsDecoration.noFrillsDeco Tabbed.shrinkText topBarTheme
--     mySpacing = Spacing.spacing gap
--     sGap = quot gap 2
--     myGaps =
--       Gaps.gaps
--         [ (WindowNavigation.U, gap)
--         , (WindowNavigation.D, gap)
--         , (WindowNavigation.L, gap)
--         , (WindowNavigation.R, gap)
--         ]
--     mySmallGaps =
--       Gaps.gaps
--         [ (WindowNavigation.U, sGap)
--         , (WindowNavigation.D, sGap)
--         , (WindowNavigation.L, sGap)
--         , (WindowNavigation.R, sGap)
--         ]
--     myBigGaps =
--       Gaps.gaps
--         [ (WindowNavigation.U, gap * 2)
--         , (WindowNavigation.D, gap * 2)
--         , (WindowNavigation.L, gap * 2)
--         , (WindowNavigation.R, gap * 2)
--         ]
--     --------------------------------------------------------------------------
--     -- Tabs Layout                                                          --
--     --------------------------------------------------------------------------
--     tabs =
--       named "Tabs" $
--       ManageDocks.avoidStruts $
--       addTopBar $ Tabbed.addTabs Tabbed.shrinkText myTabTheme $ Simplest.Simplest
--     flex =
--       trimNamed 5 "Flex" $
--       ManageDocks.avoidStruts
--               -- don't forget: even though we are using X.A.Navigation2D
--               -- we need windowNavigation for merging to sublayouts
--        $
--       WindowNavigation.windowNavigation $
--       addTopBar $
--       Tabbed.addTabs Tabbed.shrinkText myTabTheme
--               -- $ subLayout [] (Simplest ||| (mySpacing $ Accordion))
--        $
--       SubLayouts.subLayout [] (Simplest.Simplest LayoutCombinators.||| Accordion.Accordion) $
--       PerScreen.ifWider smallMonResWidth wideLayouts standardLayouts
--       where
--         wideLayouts =
--           myGaps $
--           mySpacing $
--           (suffixed "Wide 3Col" $ ThreeColumns.ThreeColMid 1 (1 / 20) (1 / 2)) LayoutCombinators.|||
--           (trimSuffixed 1 "Wide BSP" $ Hidden.hiddenWindows BinarySpacePartition.emptyBSP)
--                   --  ||| fullTabs
--         standardLayouts =
--           myGaps $
--           mySpacing $
--           (suffixed "Std 2/3" $ ResizableTile.ResizableTall 1 (1 / 20) (2 / 3) []) LayoutCombinators.|||
--           (suffixed "Std 1/2" $ ResizableTile.ResizableTall 1 (1 / 20) (1 / 2) [])
--
myModMask = XMonad.mod4Mask -- super (and on my system, hyper) keys

-- Display keyboard mappings using zenity
-- from https://github.com/thomasf/dotfiles-thomasf-xmonad/
--              blob/master/.xmonad/lib/XMonad/Config/A00001.hs
showKeybindings ::
     [((XMonad.KeyMask, XMonad.KeySym), NamedActions.NamedAction)] -> NamedActions.NamedAction
showKeybindings x =
  NamedActions.addName "Show Keybindings" $
  XMonad.io $ do
    h <- Run.spawnPipe "zenity --text-info --font=terminus"
    Run.hPutStr h (unlines $ NamedActions.showKm x)
    IO.hClose h
    return ()

wsKeys = map show $ [1 .. 9] ++ [0]

nextNonEmptyWS =
  CycleWS.findWorkspace getSortByIndexNoSP Prompt.Next CycleWS.HiddenNonEmptyWS 1 >>= \t ->
    (XMonad.windows . StackSet.view $ t)

prevNonEmptyWS =
  CycleWS.findWorkspace getSortByIndexNoSP Prompt.Prev CycleWS.HiddenNonEmptyWS 1 >>= \t ->
    (XMonad.windows . StackSet.view $ t)

getSortByIndexNoSP =
  fmap (. NamedScratchpad.namedScratchpadFilterOutWorkspace) WorkspaceCompare.getSortByIndex

myKeys conf =
  let subKeys str ks = NamedActions.subtitle str : EZConfig.mkNamedKeymap conf ks
      dirKeys = ["j", "k", "h", "l"]
      arrowKeys = ["<D>", "<U>", "<L>", "<R>"]
      dirs = [WindowNavigation.D, WindowNavigation.U, WindowNavigation.L, WindowNavigation.R]
    --screenAction f        = screenWorkspace >=> flip whenJust (windows . f)
      zipM m nm ks as f = zipWith (\k d -> (m ++ k, NamedActions.addName nm $ f d)) ks as
      zipM' m nm ks as f b = zipWith (\k d -> (m ++ k, NamedActions.addName nm $ f d b)) ks as
    -- try sending one message, fallback if unreceived, then refresh
      tryMsgR x y = sequence_ [(MessageFeedback.tryMessage_ x y), XMonad.refresh]
    -- do something with current X selection
      toggleFloat w =
        XMonad.windows
          (\s ->
             if Map.member w (StackSet.floating s)
               then StackSet.sink w s
               else (StackSet.float w (StackSet.RationalRect (1 / 3) (1 / 4) (1 / 2) (4 / 5)) s))
    -----------------------------------------------------------------------
    -- System / Utilities
    -----------------------------------------------------------------------
  in subKeys
       "System"
       [ ("M-q", NamedActions.addName "Restart XMonad" $ XMonad.spawn "xmonad --restart")
       , ( "M-C-q"
         , NamedActions.addName "Rebuild & restart XMonad" $
           XMonad.spawn "xmonad --recompile && xmonad --restart")
       , ( "M-S-q"
         , NamedActions.addName "Quit XMonad" $
           ConfirmPrompt.confirmPrompt hotPromptTheme "Quit XMonad" $
           XMonad.io (Exit.exitWith Exit.ExitSuccess))
       ] NamedActions.^++^
    -----------------------------------------------------------------------
    -- Launchers
    -----------------------------------------------------------------------
     subKeys
       "Launchers"
       [ ("M-<Space>", NamedActions.addName "Launcher" $ XMonad.spawn Conf.Applications.launcher)
       , ("M-<Return>", NamedActions.addName "Terminal" $ XMonad.spawn Conf.Applications.terminal)
       , ("M-\\", NamedActions.addName "Browser" $ XMonad.spawn Conf.Applications.browser)
       , ("M-s s", NamedActions.addName "Cancel submap" $ return ())
       , ("M-s M-s", NamedActions.addName "Cancel submap" $ return ())
       ] NamedActions.^++^
    -----------------------------------------------------------------------
    -- Windows
    -----------------------------------------------------------------------
     subKeys
       "Windows"
       ([ ("M-<Backspace>", NamedActions.addName "Kill" CopyWindow.kill1)
        , ( "M-S-<Backspace>"
          , NamedActions.addName "Kill all" $
            ConfirmPrompt.confirmPrompt hotPromptTheme "kill all" $ WithAll.killAll)
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
        zipM' "M-S-" "Swap workspace to screen" arrowKeys dirs Navigation2D.screenSwap True) NamedActions.^++^
    -----------------------------------------------------------------------
    -- Workspaces & Projects
    -----------------------------------------------------------------------
     subKeys
       "Workspaces & Projects"
       ([ ( "M-w"
          , NamedActions.addName "Switch to Project" $
            DynamicProjects.switchProjectPrompt warmPromptTheme)
        , ( "M-S-w"
          , NamedActions.addName "Shift to Project" $
            DynamicProjects.shiftToProjectPrompt warmPromptTheme)
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
          (DynamicWorkspaces.withNthWorkspace CopyWindow.copy)) NamedActions.^++^
    -- TODO: consider a submap for nav/move to specific workspaces based on first initial
    -----------------------------------------------------------------------
    -- Layouts & Sublayouts
    -----------------------------------------------------------------------
     subKeys
       "Layout Management"
       [ ( "M-<Tab>"
         , NamedActions.addName "Cycle all layouts" $ XMonad.sendMessage XMonad.NextLayout)
       , ("M-C-<Tab>", NamedActions.addName "Cycle sublayout" $ SubLayouts.toSubl XMonad.NextLayout)
       , ( "M-S-<Tab>"
         , NamedActions.addName "Reset layout" $ XMonad.setLayout $ XMonad.layoutHook conf)
       , ("M-y", NamedActions.addName "Float tiled w" $ XMonad.withFocused toggleFloat)
       , ("M-S-y", NamedActions.addName "Tile all floating w" $ WithAll.sinkAll)
       , ( "M-,"
         , NamedActions.addName "Decrease master windows" $
           XMonad.sendMessage (XMonad.IncMasterN (-1)))
       , ( "M-."
         , NamedActions.addName "Increase master windows" $ XMonad.sendMessage (XMonad.IncMasterN 1))
       , ( "M-r"
         , NamedActions.addName "Reflect/Rotate" $
           tryMsgR (BinarySpacePartition.Rotate) (MultiToggle.Toggle Reflect.REFLECTX))
       , ( "M-S-r"
         , NamedActions.addName "Force Reflect (even on BSP)" $
           XMonad.sendMessage (MultiToggle.Toggle Reflect.REFLECTX))
    -- If following is run on a floating window, the sequence first tiles it.
    -- Not perfect, but works.
       , ( "M-f"
         , NamedActions.addName "Fullscreen" $
           sequence_
             [ (XMonad.withFocused $ XMonad.windows . StackSet.sink)
             , (XMonad.sendMessage $ MultiToggle.Toggle MultiToggle.Instances.FULL)
             ])
    -- Fake fullscreen fullscreens into the window rect. The expand/shrink
    -- is a hack to make the full screen paint into the rect properly.
    -- The tryMsgR handles the BSP vs standard resizing functions.
       , ( "M-S-f"
         , NamedActions.addName "Fake fullscreen" $
           sequence_
             [ (Paste.sendKey Paste.noModMask XMonad.xK_F11)
             , (tryMsgR (BinarySpacePartition.ExpandTowards WindowNavigation.L) (XMonad.Shrink))
             , (tryMsgR (BinarySpacePartition.ExpandTowards WindowNavigation.R) (XMonad.Expand))
             ])
       , ( "C-S-h"
         , NamedActions.addName "Ctrl-h passthrough" $ Paste.sendKey XMonad.controlMask XMonad.xK_h)
       , ( "C-S-j"
         , NamedActions.addName "Ctrl-j passthrough" $ Paste.sendKey XMonad.controlMask XMonad.xK_j)
       , ( "C-S-k"
         , NamedActions.addName "Ctrl-k passthrough" $ Paste.sendKey XMonad.controlMask XMonad.xK_k)
       , ( "C-S-l"
         , NamedActions.addName "Ctrl-l passthrough" $ Paste.sendKey XMonad.controlMask XMonad.xK_l)
       ] NamedActions.^++^
    -----------------------------------------------------------------------
    -- Resizing
    -----------------------------------------------------------------------
     subKeys
       "Resize"
       [ ( "M-["
         , NamedActions.addName "Expand (L on BSP)" $
           tryMsgR (BinarySpacePartition.ExpandTowards WindowNavigation.L) (XMonad.Shrink))
       , ( "M-]"
         , NamedActions.addName "Expand (R on BSP)" $
           tryMsgR (BinarySpacePartition.ExpandTowards WindowNavigation.R) (XMonad.Expand))
       , ( "M-S-["
         , NamedActions.addName "Expand (U on BSP)" $
           tryMsgR
             (BinarySpacePartition.ExpandTowards WindowNavigation.U)
             (ResizableTile.MirrorShrink))
       , ( "M-S-]"
         , NamedActions.addName "Expand (D on BSP)" $
           tryMsgR
             (BinarySpacePartition.ExpandTowards WindowNavigation.D)
             (ResizableTile.MirrorExpand))
       , ( "M-C-["
         , NamedActions.addName "Shrink (L on BSP)" $
           tryMsgR (BinarySpacePartition.ShrinkFrom WindowNavigation.R) (XMonad.Shrink))
       , ( "M-C-]"
         , NamedActions.addName "Shrink (R on BSP)" $
           tryMsgR (BinarySpacePartition.ShrinkFrom WindowNavigation.L) (XMonad.Expand))
       , ( "M-C-S-["
         , NamedActions.addName "Shrink (U on BSP)" $
           tryMsgR (BinarySpacePartition.ShrinkFrom WindowNavigation.D) (ResizableTile.MirrorShrink))
       , ( "M-C-S-]"
         , NamedActions.addName "Shrink (D on BSP)" $
           tryMsgR (BinarySpacePartition.ShrinkFrom WindowNavigation.U) (ResizableTile.MirrorExpand))
       ]
    -----------------------------------------------------------------------
    -- Screens
    -----------------------------------------------------------------------

-- Mouse bindings: default actions bound to mouse events
-- Includes window snapping on move/resize using X.A.FloatSnap
-- Includes window w/h ratio constraint (square) using X.H.ConstrainedResize
myMouseBindings (XMonad.XConfig {XMonad.modMask = myModMask}) =
  Map.fromList $
  [ ( (myModMask, XMonad.button1)
    , (\w ->
         XMonad.focus w >> XMonad.mouseMoveWindow w >>
         FloatSnap.ifClick (FloatSnap.snapMagicMove (Just 50) (Just 50) w) >>
         XMonad.windows StackSet.shiftMaster))
  , ( (myModMask XMonad..|. XMonad.shiftMask, XMonad.button1)
    , (\w ->
         XMonad.focus w >> XMonad.mouseMoveWindow w >>
         FloatSnap.ifClick
           (FloatSnap.snapMagicResize
              [WindowNavigation.L, WindowNavigation.R, WindowNavigation.U, WindowNavigation.D]
              (Just 50)
              (Just 50)
              w) >>
         XMonad.windows StackSet.shiftMaster))
  , ( (myModMask, XMonad.button3)
    , (\w ->
         XMonad.focus w >> XMonad.mouseResizeWindow w >>
         FloatSnap.ifClick
           (FloatSnap.snapMagicResize [WindowNavigation.R, WindowNavigation.D] (Just 50) (Just 50) w) >>
         XMonad.windows StackSet.shiftMaster))
  , ( (myModMask XMonad..|. XMonad.shiftMask, XMonad.button3)
    , (\w ->
         XMonad.focus w >> ConstrainedResize.mouseResizeWindow w True >>
         FloatSnap.ifClick
           (FloatSnap.snapMagicResize [WindowNavigation.R, WindowNavigation.D] (Just 50) (Just 50) w) >>
         XMonad.windows StackSet.shiftMaster))
  ]

--    , ((mySecondaryModMask,      button4), (\w -> focus w
--      >> prevNonEmptyWS))
--
--    , ((mySecondaryModMask,      button5), (\w -> focus w
--      >> nextNonEmptyWS))
------------------------------------------------------------------------}}}
-- Startup                                                              {{{
---------------------------------------------------------------------------
myStartupHook
    -- init-tilingwm sets up all major "desktop environment" like components
    -- spawnOnce "$HOME/bin/wm/init-tilingwm"
    -- spawn "/home/ethan/bin/wm/init-tilingwm"
  -- spawn "/home/ethan/bin/wm/init-wallpaper"
    -- init-tray kills and restarts stalone tray, hence just "spawn" so it
    -- runs on restart and will suffice to reposition tray on display changes
    -- TODO: evaluate moving to a "restart tray only" option on display change
    -- spawn     "$HOME/bin/wm/init-tray"
 = do
  Cursor.setDefaultCursor Cursor.xC_left_ptr

------------------------------------------------------------------------}}}
-- Log                                                                  {{{
---------------------------------------------------------------------------
myLogHook h
    -- following block for copy windows marking
 = do
  copies <- CopyWindow.wsContainingCopies
  let check ws
        | ws `elem` copies =
          DynamicLog.pad . DynamicLog.xmobarColor yellow red . DynamicLog.wrap "*" " " $ ws
        | otherwise = DynamicLog.pad ws
  FadeWindows.fadeWindowsLogHook myFadeHook
  EwmhDesktops.ewmhDesktopsLogHook
    --dynamicLogWithPP $ defaultPP
  DynamicLog.dynamicLogWithPP $
    Prompt.def
    { DynamicLog.ppCurrent = DynamicLog.xmobarColor active "" . DynamicLog.wrap "[" "]"
    , DynamicLog.ppTitle = DynamicLog.xmobarColor active "" . DynamicLog.shorten 50
    , DynamicLog.ppVisible = DynamicLog.xmobarColor base0 "" . DynamicLog.wrap "(" ")"
    , DynamicLog.ppUrgent = DynamicLog.xmobarColor red "" . DynamicLog.wrap " " " "
    , DynamicLog.ppHidden = check
    , DynamicLog.ppHiddenNoWindows = const ""
    , DynamicLog.ppSep = DynamicLog.xmobarColor red blue "  :  "
    , DynamicLog.ppWsSep = " "
    , DynamicLog.ppLayout = DynamicLog.xmobarColor yellow ""
    , DynamicLog.ppOrder = id
    , DynamicLog.ppOutput = Run.hPutStrLn h
    , DynamicLog.ppSort =
        fmap (NamedScratchpad.namedScratchpadFilterOutWorkspace .) (DynamicLog.ppSort Prompt.def)
    , DynamicLog.ppExtras = []
    }

myFadeHook =
  XMonad.composeAll
    [ FadeWindows.opaque -- default to opaque
    , FadeWindows.isUnfocused XMonad.--> FadeWindows.opacity 0.85
    , (XMonad.className XMonad.=? "Terminator") XMonad.<&&> (FadeWindows.isUnfocused) XMonad.-->
      FadeWindows.opacity 0.9
    , (XMonad.className XMonad.=? "URxvt") XMonad.<&&> (FadeWindows.isUnfocused) XMonad.-->
      FadeWindows.opacity 0.9
    , fmap ("Google" `List.isPrefixOf`) XMonad.className XMonad.--> FadeWindows.opaque
    , ManageHelpers.isDialog XMonad.--> FadeWindows.opaque
    --, isUnfocused --> opacity 0.55
    --, isFloating  --> opacity 0.75
    ]

------------------------------------------------------------------------}}}
-- Actions                                                              {{{
---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- Urgency Hook
---------------------------------------------------------------------------
-- from https://pbrisbin.com/posts/using_notify_osd_for_xmonad_notifications/
data LibNotifyUrgencyHook =
  LibNotifyUrgencyHook
  deriving (Read, Show)

instance UrgencyHook.UrgencyHook LibNotifyUrgencyHook where
  urgencyHook LibNotifyUrgencyHook w = do
    name <- NamedWindows.getName w
    Just idx <- fmap (StackSet.findTag w) $ XMonad.gets XMonad.windowset
    Run.safeSpawn "notify-send" [show name, "workspace " ++ idx]

---------------------------------------------------------------------------
-- New Window Actions
---------------------------------------------------------------------------
-- https://wiki.haskell.org/Xmonad/General_xmonad.hs_config_tips#ManageHook_examples
-- <+> manageHook defaultConfig
-- myManageHook :: XMonad.ManageHook
-- myManageHook =
--   manageSpecific XMonad.<+> ManageDocks.manageDocks XMonad.<+> Fullscreen.fullscreenManageHook XMonad.<+>
--   SpawnOn.manageSpawn
--   where
--     manageSpecific =
--       ManageHelpers.composeOne
--         [ XMonad.resource XMonad.=? "desktop_window" ManageHelpers.-?> XMonad.doIgnore
--         , XMonad.resource XMonad.=? "stalonetray" ManageHelpers.-?> XMonad.doIgnore
--         , XMonad.resource XMonad.=? "vlc" ManageHelpers.-?> XMonad.doFloat
--             -- , resource =? trelloResource -?> doFullFloat
--             -- , resource =? trelloWorkResource -?> doFullFloat
--             -- , resource =? googleMusicResource -?> doFullFloat
--             -- , resource =? plexResource -?> doCenterFloat
--             -- , resource =? hangoutsResource -?> insertPosition End Newer
--         , ManageHelpers.transience
--         , isBrowserDialog ManageHelpers.-?> forceCenterFloat
--             --, isConsole -?> forceCenterFloat
--         , isRole XMonad.=? gtkFile ManageHelpers.-?> forceCenterFloat
--         , ManageHelpers.isDialog ManageHelpers.-?> ManageHelpers.doCenterFloat
--         , isRole XMonad.=? "pop-up" ManageHelpers.-?> ManageHelpers.doCenterFloat
--         , ManageHelpers.isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH" ManageHelpers.-?>
--           ManageHelpers.doCenterFloat
--         , XMonad.resource XMonad.=? "console" ManageHelpers.-?> tileBelowNoFocus
--         , ManageHelpers.isFullscreen ManageHelpers.-?> ManageHelpers.doFullFloat
--         , pure True ManageHelpers.-?> tileBelow
--         ]
--     isBrowserDialog =
--       ManageHelpers.isDialog XMonad.<&&> XMonad.className XMonad.=? Conf.Applications.browserClass
--     gtkFile = "GtkFileChooserDialog"
--     isRole = XMonad.stringProperty "WM_WINDOW_ROLE"
--         -- insert WHERE and focus WHAT
--     tileBelow = InsertPosition.insertPosition InsertPosition.Below InsertPosition.Newer
--     tileBelowNoFocus = InsertPosition.insertPosition InsertPosition.Below InsertPosition.Older

---------------------------------------------------------------------------
-- X Event Actions
---------------------------------------------------------------------------
-- for reference, the following line is the same as dynamicTitle myDynHook
-- <+> dynamicPropertyChange "WM_NAME" myDynHook
-- I'm not really into full screens without my say so... I often like to
-- fullscreen a window but keep it constrained to a window rect (e.g.
-- for videos, etc. without the UI chrome cluttering things up). I can
-- always do that and then full screen the subsequent window if I want.
-- THUS, to cut a long comment short, no fullscreenEventHook
-- <+> XMonad.Hooks.EwmhDesktops.fullscreenEventHook
-- myHandleEventHook =
--   ManageDocks.docksEventHook XMonad.<+> FadeWindows.fadeWindowsEventHook XMonad.<+>
--   XMonad.handleEventHook Prompt.def XMonad.<+>
--   Fullscreen.fullscreenEventHook

---------------------------------------------------------------------------
-- Custom hook helpers
---------------------------------------------------------------------------
-- from:
-- https://github.com/pjones/xmonadrc/blob/master/src/XMonad/Local/Action.hs
--
-- Useful when a floating window requests stupid dimensions.  There
-- was a bug in Handbrake that would pop up the file dialog with
-- almost no height due to one of my rotated monitors.
forceCenterFloat :: XMonad.ManageHook
forceCenterFloat = ManageHelpers.doFloatDep move
  where
    move :: StackSet.RationalRect -> StackSet.RationalRect
    move _ = StackSet.RationalRect x y w h
    w, h, x, y :: Rational
    w = 1 / 3
    h = 1 / 2
    x = (1 - w) / 2
    y = (1 - h) / 2
-- vim: ft=haskell:foldmethod=marker:expandtab:ts=4:shiftwidth=4
