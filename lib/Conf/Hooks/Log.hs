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
import Data.List
        ( sortBy
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

import qualified XMonad.Hooks.DynamicLog as DynamicLog
import qualified XMonad.Hooks.EwmhDesktops as EwmhDesktops
import qualified XMonad.Hooks.FadeWindows as FadeWindows

import XMonad.Core
        ( X
        , WindowSpace
        )

import XMonad.StackSet
        ( tag
        )

import XMonad.Util.Run
        ( unsafeSpawn
        )

import XMonad.Hooks.DynamicLog as DynamicLog
        ( xmobarColor
        , shorten
        )

import XMonad.Layout.IndependentScreens
        ( countScreens
        , whenCurrentOn
        , unmarshall
        , unmarshallS
        , unmarshallWindowSpace
        , marshallWindowSpace
        )

import XMonad.Util.NamedScratchpad
        ( namedScratchpadFilterOutWorkspace
        )

type FocusPipe      = FilePath
type WorkspacesPipe = FilePath
data XMobarOption   = XMobarOption XMonad.ScreenId FocusPipe WorkspacesPipe

log :: [XMobarOption] -> X ()
log bars
 = do
  FadeWindows.fadeWindowsLogHook Hooks.Fade.fade
  EwmhDesktops.ewmhDesktopsLogHook
  forM_ bars $ \(XMobarOption sid focusPipe workspacesPipe) -> do
    DynamicLog.dynamicLogWithPP $ ppFocus      focusPipe      sid
    DynamicLog.dynamicLogWithPP $ ppWorkspaces workspacesPipe sid

ppFocus :: FilePath -> XMonad.ScreenId -> DynamicLog.PP
ppFocus pipe sid =
  whenCurrentOn sid
    DynamicLog.def
      { DynamicLog.ppTitle   = xmobarColor Colors.green   "" . DynamicLog.shorten 50
      , DynamicLog.ppLayout  = xmobarColor Colors.violet  ""
      , DynamicLog.ppSep     = xmobarColor Colors.base02  "" " | "
      , DynamicLog.ppOrder   = \(_ws:layout:title:_) -> [title, layout]
      , DynamicLog.ppOutput  = appendFile pipe . decodeString . (++ "\n")
      }

ppWorkspaces' :: DynamicLog.PP
ppWorkspaces' =
    DynamicLog.def
      { DynamicLog.ppCurrent         = xmobarColor Colors.orange  "" . DynamicLog.wrap "[" "]"
      , DynamicLog.ppVisible         = xmobarColor Colors.green   "" . DynamicLog.wrap "(" ")"
      , DynamicLog.ppUrgent          = xmobarColor Colors.red     "" . DynamicLog.wrap "#" "#"
      , DynamicLog.ppHidden          = xmobarColor Colors.cyan    "" . DynamicLog.wrap " " " "
      , DynamicLog.ppHiddenNoWindows = xmobarColor Colors.blue    "" . DynamicLog.wrap " " " "
      , DynamicLog.ppWsSep           = xmobarColor Colors.base02  "" "/"
      , DynamicLog.ppLayout          = xmobarColor Colors.violet  ""
      , DynamicLog.ppOrder           = \(ws:_layout:_title:_) -> [ws]
      }

ppWorkspaces :: FilePath -> XMonad.ScreenId -> DynamicLog.PP
ppWorkspaces pipe sid =
    ppWorkspaces'
      { DynamicLog.ppCurrent         = DynamicLog.ppCurrent ppWorkspaces' . snd . unmarshall
      , DynamicLog.ppVisible         = DynamicLog.ppVisible ppWorkspaces' . snd . unmarshall
      , DynamicLog.ppUrgent          = DynamicLog.ppUrgent  ppWorkspaces' . snd . unmarshall
      , DynamicLog.ppHidden          = DynamicLog.ppHidden  ppWorkspaces' . snd . unmarshall
      , DynamicLog.ppHiddenNoWindows = DynamicLog.ppHiddenNoWindows ppWorkspaces' . snd . unmarshall
      , DynamicLog.ppSort            = fmap (do return $ wsFilter sid) (DynamicLog.ppSort ppWorkspaces')
      , DynamicLog.ppOutput          = appendFile pipe . decodeString . (++ "\n")
      }

wsFilter :: XMonad.ScreenId -> [WindowSpace] -> [WindowSpace]
wsFilter s = pScreens . vSort . vScreens . namedScratchpadFilterOutWorkspace where
    onScreen ws = unmarshallS (tag ws) == s
    vScreens    = map unmarshallWindowSpace . filter onScreen
    pScreens    = map (marshallWindowSpace s)
    vSort       = sortBy (\a b -> compare (tag a) (tag b))

initBars :: IO [XMobarOption]
initBars
  = do
    numScreens <- countScreens
    barOptions <- forM [0 .. numScreens - 1] $ \sid -> do
      focusPipe      <- getTempFifo "xmobar-focus-"
      workspacesPipe <- getTempFifo "xmobar-workspaces-"
      return $ XMobarOption sid focusPipe workspacesPipe
    unsafeSpawn $ initCommand barOptions
    return barOptions

initCommand :: [XMobarOption] -> String
initCommand xs = "$HOME/.xmonad/initbars.sh " ++ (unwords $ xmobarCommands xs)

xmobarCommands :: [XMobarOption] -> [String]
xmobarCommands = map xmobarCommand

xmobarCommand :: XMobarOption -> String
xmobarCommand (XMobarOption (XMonad.S sid) focusPipe workspacesPipe) =
  unwords [show sid, focusPipe, workspacesPipe]

-- TODO: Move out of module
getTempFifo :: String -> IO FilePath
getTempFifo prefix = do
  tmpDir <- getTemporaryDirectory
  (tmpFile, h) <- openTempFile tmpDir prefix
  hClose h
  removeFile tmpFile
  createNamedPipe tmpFile $ unionFileModes ownerReadMode ownerWriteMode
  return tmpFile
