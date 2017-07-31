{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable,
  TypeSynonymInstances, MultiParamTypeClasses #-}

----------------------------------------------------------------------------
-- |
-- Module       : Conf.Hooks.Log
-- Copyright    : (c) maddy@na.ai
-- License      : MIT
--
-- Maintainer   : maddy@na.ai
-- Stability    : unstable
-- Portability  : unportable
--
----------------------------------------------------------------------------
module Conf.Hooks.Log
  ( log
  , FocusPipe
  , WorkspacesPipe
  , XMobarOption
  , xmobarCommand
  , getTempFifo
  , initBars
  ) where

import Prelude hiding (log)
import Control.Monad
        ( replicateM
        , forM_
        , forM
        )
import Codec.Binary.UTF8.String
        ( decodeString
        )
import System.Directory
        ( getTemporaryDirectory
        , removeFile
        )
import System.IO
        ( FilePath
        , hClose
        , openTempFile
        )
import System.Posix.Files
        ( createNamedPipe
        , ownerReadMode
        , ownerWriteMode
        , unionFileModes
        )

import qualified Conf.Theme.Colors as Colors
import qualified Conf.Hooks.Fade as Hooks.Fade

import qualified XMonad

import qualified XMonad.Layout.IndependentScreens as IndependentScreens

import qualified XMonad.Hooks.DynamicLog as DynamicLog
import qualified XMonad.Hooks.EwmhDesktops as EwmhDesktops
import qualified XMonad.Hooks.FadeWindows as FadeWindows

import qualified XMonad.Util.NamedScratchpad as NamedScratchpad

import XMonad.Util.Run
        ( unsafeSpawn
        )

type FocusPipe      = FilePath
type WorkspacesPipe = FilePath
data XMobarOption   = XMobarOption XMonad.ScreenId FocusPipe WorkspacesPipe

log bars
 = do
  FadeWindows.fadeWindowsLogHook Hooks.Fade.fade
  EwmhDesktops.ewmhDesktopsLogHook
  forM_ bars $ \(XMobarOption sid foc wss) -> do
    let ppFoc = ppFocus      foc sid
    let ppWss = ppWorkspaces wss sid
    DynamicLog.dynamicLogWithPP ppFoc
    DynamicLog.dynamicLogWithPP ppWss

ppFocus :: FilePath -> XMonad.ScreenId -> DynamicLog.PP
ppFocus foc (XMonad.S sid) =
  IndependentScreens.whenCurrentOn (XMonad.S sid)
    DynamicLog.def
      { DynamicLog.ppCurrent = DynamicLog.xmobarColor Colors.orange  "" . DynamicLog.wrap "[" "]"
      , DynamicLog.ppTitle   = DynamicLog.xmobarColor Colors.orange  "" . DynamicLog.shorten 50
      , DynamicLog.ppOrder   = \(_:_:title:_) -> [title]
      , DynamicLog.ppOutput  = appendFile foc . decodeString . (++ "\n")
      -- , DynamicLog.ppOutput  = appendFile "$HOME/.local/tmp/xmonad-foc" . decodeString . (++ "\n")
      }


ppWorkspaces :: FilePath -> XMonad.ScreenId -> DynamicLog.PP
ppWorkspaces wss (XMonad.S sid) =
  IndependentScreens.marshallPP (XMonad.S sid)
    DynamicLog.def
      { DynamicLog.ppCurrent         = DynamicLog.xmobarColor Colors.green   "" . DynamicLog.wrap "[" "]"
      , DynamicLog.ppVisible         = DynamicLog.xmobarColor Colors.skyblue "" . DynamicLog.wrap "(" ")"
      , DynamicLog.ppTitle           = DynamicLog.xmobarColor Colors.green   "" . DynamicLog.shorten 50
      , DynamicLog.ppLayout          = DynamicLog.xmobarColor Colors.violet  ""
      , DynamicLog.ppUrgent          = DynamicLog.xmobarColor Colors.red     "" . DynamicLog.wrap " " " "
      , DynamicLog.ppHidden          = DynamicLog.xmobarColor Colors.blue    ""
      , DynamicLog.ppWsSep           = DynamicLog.xmobarColor Colors.base02  "" " / "
      , DynamicLog.ppSep             = DynamicLog.xmobarColor Colors.base02  "" "  |  "
      , DynamicLog.ppHiddenNoWindows = const ""
      , DynamicLog.ppOrder           = \(ws:_:_:_) -> [ws]
      , DynamicLog.ppOutput          = appendFile wss . decodeString . (++ "\n")
      -- , DynamicLog.ppOutput  = appendFile "$HOME/.local/tmp/xmonad-wss" . decodeString . (++ "\n")
      }

initBars --numScreens
  = do
    numScreens <- IndependentScreens.countScreens
    barOptions <- forM [0 .. numScreens - 1] $ \sid -> do
                    foc <- getTempFifo ("xmobar-foc-" ++ show sid ++ "-")
                    wss <- getTempFifo ("xmobar-wss-" ++ show sid ++ "-")
                    return $ XMobarOption sid foc wss
    mapM_ (unsafeSpawn . xmobarCommand) barOptions
    return barOptions

xmobarCommand :: XMobarOption -> String
xmobarCommand (XMobarOption (XMonad.S sid) foc wss) =
  "\
  \xmobar \
    \ -x " ++ show sid ++ "\
    \ -C '[Run PipeReader \"" ++ foc ++ "\" \"focus\", \
         \Run PipeReader \"" ++ wss ++ "\" \"workspaces\"]' \
    \ $HOME/.xmonad/xmobar.conf"

statusBarConf = "$HOME/.xmonad/xmobar.conf"

-- TODO: Move out of module
getTempFifo :: String -> IO FilePath
getTempFifo prefix = do
  tmpDir <- getTemporaryDirectory
  (tmpFile, h) <- openTempFile tmpDir prefix
  hClose h
  removeFile tmpFile
  createNamedPipe tmpFile $ unionFileModes ownerReadMode ownerWriteMode
  return tmpFile

-- -- dyLogUnfocus :: XMonad.ScreenId -> DynamicLog.PP
-- -- dyLogUnfocus n =
-- dyLogUnfocus :: DynamicLog.PP
-- dyLogUnfocus =
--   -- IndependentScreens.marshallPP n
--   DynamicLog.def
--     -- Workspaces
--     { DynamicLog.ppCurrent         = DynamicLog.xmobarColor Colors.green   "" . DynamicLog.wrap "[" "]"
--     , DynamicLog.ppVisible         = DynamicLog.xmobarColor Colors.skyblue "" . DynamicLog.wrap "(" ")"
--     , DynamicLog.ppUrgent          = DynamicLog.xmobarColor Colors.red     "" . DynamicLog.wrap " " " "
--     , DynamicLog.ppHidden          = DynamicLog.xmobarColor Colors.blue    ""
--     , DynamicLog.ppHiddenNoWindows = const ""
--
--     -- Main
--     , DynamicLog.ppTitle           = DynamicLog.xmobarColor Colors.green   "" . DynamicLog.shorten 50
--     , DynamicLog.ppLayout          = DynamicLog.xmobarColor Colors.violet  ""
--
--     -- Seps
--     , DynamicLog.ppWsSep           = DynamicLog.xmobarColor Colors.base02  "" " / "
--     , DynamicLog.ppSep             = DynamicLog.xmobarColor Colors.base02  "" "  |  "
--
--     -- Mods
--     , DynamicLog.ppOrder           = id
--     , DynamicLog.ppExtras          = []
--     , DynamicLog.ppSort            = fmap
--                                       (NamedScratchpad.namedScratchpadFilterOutWorkspace .)
--                                       (DynamicLog.ppSort DynamicLog.def)
--     }
--
-- dyLogFocus :: DynamicLog.PP
-- dyLogFocus =
-- -- dyLogFocus :: XMonad.ScreenId -> DynamicLog.PP
-- -- dyLogFocus n =
--   -- dyLogUnfocus n
--   dyLogUnfocus
--     { DynamicLog.ppCurrent = DynamicLog.xmobarColor Colors.orange "" . DynamicLog.wrap "[" "]"
--     , DynamicLog.ppTitle   = DynamicLog.xmobarColor Colors.orange "" . DynamicLog.shorten 50
--     }

