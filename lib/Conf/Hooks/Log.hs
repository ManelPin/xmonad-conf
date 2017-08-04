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
  , initBars
  ) where

import Prelude hiding (log)

import Control.Monad
        ( forM_
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

type Pipe         = FilePath
data XMobarOption = XMobarOption XMonad.ScreenId Pipe

log bars
 = do
  FadeWindows.fadeWindowsLogHook Hooks.Fade.fade
  EwmhDesktops.ewmhDesktopsLogHook
  forM_ bars $ \(XMobarOption sid pipe) -> do
    let pp = ppWorkspaces pipe sid
    DynamicLog.dynamicLogWithPP pp

-- ppFocus :: FilePath -> XMonad.ScreenId -> DynamicLog.PP
-- ppFocus foc (XMonad.S sid) =
--   IndependentScreens.whenCurrentOn (XMonad.S sid)
--     DynamicLog.def
--       { DynamicLog.ppCurrent = DynamicLog.xmobarColor Colors.orange  "" . DynamicLog.wrap "[" "]"
--       , DynamicLog.ppTitle   = DynamicLog.xmobarColor Colors.orange  "" . DynamicLog.shorten 50
--       , DynamicLog.ppOrder   = \(_:_:title:_) -> [title]
--       , DynamicLog.ppOutput  = appendFile foc . decodeString . (++ "\n")
--       -- , DynamicLog.ppOutput  = appendFile "$HOME/.local/tmp/xmonad-foc" . decodeString . (++ "\n")
--       }


ppWorkspaces :: FilePath -> XMonad.ScreenId -> DynamicLog.PP
ppWorkspaces pipe (XMonad.S _) =
  -- IndependentScreens.marshallPP (XMonad.S sid)
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
      , DynamicLog.ppOrder           = id -- \(ws:_:_:_) -> [ws]
      -- , DynamicLog.ppOutput          = hPutStrLn handle --appendFile pipe . decodeString . (++ "\n")
      , DynamicLog.ppOutput          = appendFile pipe . decodeString . (++ "\n")
      -- , DynamicLog.ppOutput  = appendFile "$HOME/.local/tmp/xmonad-wss" . decodeString . (++ "\n")
      , DynamicLog.ppSort            = fmap
                                        (NamedScratchpad.namedScratchpadFilterOutWorkspace .)
                                        (DynamicLog.ppSort DynamicLog.def)
      }

initBars :: IO [XMobarOption]
initBars
  = do
    numScreens <- IndependentScreens.countScreens
    barOptions <- forM [0 .. numScreens - 1] $ \sid -> do
                    pipe <- getTempFifo "xmobar-"
                    return $ XMobarOption sid pipe
    unsafeSpawn $ initCommand barOptions
    return barOptions

initCommand :: [XMobarOption] -> String
initCommand xs = "$HOME/.xmonad/initbars.sh " ++ (unwords $ xmobarCommands xs)

xmobarCommands :: [XMobarOption] -> [String]
xmobarCommands = map xmobarCommand

xmobarCommand :: XMobarOption -> String
xmobarCommand (XMobarOption (XMonad.S sid) pipe) =
  show sid ++ " " ++ pipe

-- TODO: Move out of module
getTempFifo :: String -> IO FilePath
getTempFifo prefix = do
  tmpDir <- getTemporaryDirectory
  (tmpFile, h) <- openTempFile tmpDir prefix
  hClose h
  removeFile tmpFile
  createNamedPipe tmpFile $ unionFileModes ownerReadMode ownerWriteMode
  return tmpFile
