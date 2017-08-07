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

-- import qualified XMonad.Layout.IndependentScreens as IndependentScreens
import XMonad.Layout.IndependentScreens
        ( PhysicalWorkspace
        , VirtualWorkspace
        , countScreens
        , whenCurrentOn
        )

import qualified XMonad.Hooks.DynamicLog as DynamicLog
import qualified XMonad.Hooks.EwmhDesktops as EwmhDesktops
import qualified XMonad.Hooks.FadeWindows as FadeWindows

import qualified XMonad.Util.NamedScratchpad as NamedScratchpad

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

import Control.Arrow
        ( (***)
        )

type FocusPipe      = FilePath
type WorkspacesPipe = FilePath
data XMobarOption   = XMobarOption XMonad.ScreenId FocusPipe WorkspacesPipe

log :: [XMobarOption] -> XMonad.X ()
log bars
 = do
  FadeWindows.fadeWindowsLogHook Hooks.Fade.fade
  EwmhDesktops.ewmhDesktopsLogHook
  forM_ bars $ \(XMobarOption sid focusPipe workspacesPipe) -> do
    DynamicLog.dynamicLogWithPP $ ppFocus      focusPipe      sid
    DynamicLog.dynamicLogWithPP $ ppWorkspaces workspacesPipe sid

ppFocus :: FilePath -> XMonad.ScreenId -> DynamicLog.PP
ppFocus pipe (XMonad.S sid) =
  whenCurrentOn (XMonad.S sid)
    DynamicLog.def
      { DynamicLog.ppTitle  = xmobarColor "green" "" . shorten 45
      , DynamicLog.ppOrder  = \(_ws:_layout:title:_) -> [title]
      , DynamicLog.ppOutput = appendFile pipe . decodeString . (++ "\n")
      }

ppWorkspaces :: FilePath -> XMonad.ScreenId -> DynamicLog.PP
ppWorkspaces pipe sid =
  -- IndependentScreens.marshallPP (XMonad.S sid)
    DynamicLog.def
      { DynamicLog.ppCurrent         = xmobarColor "green" ""
      , DynamicLog.ppVisible         = xmobarColor "white" ""
      , DynamicLog.ppHiddenNoWindows = xmobarColor "#006666" ""
      , DynamicLog.ppOrder           = \(ws:_layout:_title:_) -> [ws]
      , DynamicLog.ppSort            = wsSort sid
      , DynamicLog.ppOutput          = appendFile pipe . decodeString . (++ "\n")
      }

wsSort :: XMonad.ScreenId -> X ([WindowSpace] -> [WindowSpace])
wsSort s
  = do
    -- return $ map unmarshallWindowSpace --marshallSort s id
    return $ unmarshallFilter s --marshallSort s id

unmarshallFilter :: XMonad.ScreenId -> ([WindowSpace] -> [WindowSpace])
-- marshallSort s vSort = pScreens . vSort . vScreens where
-- marshallSort s vSort = vSort . vScreens where
unmarshallFilter s = map unmarshallWindowSpace . filter onScreen --vScreens
  where
    onScreen ws = unmarshallS (tag ws) == s
    -- onScreen ws = True
    -- vScreens    = map unmarshallWindowSpace . filter onScreen
    -- pScreens    = map (marshallWindowSpace s)

-- | Convert the tag of the 'WindowSpace' from a 'PhysicalWorkspace' to a 'VirtualWorkspace'.
unmarshallWindowSpace :: WindowSpace -> WindowSpace
unmarshallWindowSpace ws = ws { tag = unmarshallW (tag ws) }

-- Unmarshall, returning just the Virtual tag
unmarshallW :: PhysicalWorkspace -> VirtualWorkspace
unmarshallW = snd . unmarshall

-- Unmarshall, returning just the ScreenId
unmarshallS :: PhysicalWorkspace -> XMonad.ScreenId
unmarshallS = fst . unmarshall

-- Convert a PhysicalWorkspace to a VirtualWorkspace plus its Xinerama ScreenID
unmarshall  :: PhysicalWorkspace -> (XMonad.ScreenId, VirtualWorkspace)
unmarshall  = ((XMonad.S . read) *** drop 1) . break (=='_')

-- -- If vSort is a function that sorts WindowSpaces with virtual names,
-- -- then marshallSort s vSort is a function which sorts WindowSpaces with
-- -- physical names in an analogous way -- but keeps
-- -- only the spaces on screen s.
-- marshallSort :: XMonad.ScreenId -> ([WindowSpace] -> [WindowSpace]) -> ([WindowSpace] -> [WindowSpace])
-- -- marshallSort s vSort = pScreens . vSort . vScreens where
-- -- marshallSort s vSort = vSort . vScreens where
-- marshallSort s vSort = vScreens where
--     -- onScreen ws = True
--     -- onScreen ws = unmarshallS (tag ws) == s
--     -- vScreens    = map unmarshallWindowSpace . filter onScreen
--     vScreens    = map unmarshallWindowSpace
--     -- pScreens    = map (marshallWindowSpace s)

marshallWindowSpace :: XMonad.ScreenId -> WindowSpace -> WindowSpace
marshallWindowSpace s ws = ws { tag = marshall s  (tag ws) }

marshall :: XMonad.ScreenId -> VirtualWorkspace -> PhysicalWorkspace
marshall (XMonad.S sc) vws = show sc ++ '_':vws


---

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
  -- show sid ++ " " ++ focusPipe ++

-- TODO: Move out of module
getTempFifo :: String -> IO FilePath
getTempFifo prefix = do
  tmpDir <- getTemporaryDirectory
  (tmpFile, h) <- openTempFile tmpDir prefix
  hClose h
  removeFile tmpFile
  createNamedPipe tmpFile $ unionFileModes ownerReadMode ownerWriteMode
  return tmpFile


-- ppWorkspaces :: FilePath -> XMonad.ScreenId -> DynamicLog.PP
-- ppWorkspaces pipe (XMonad.S sid) =
--   -- IndependentScreens.marshallPP (XMonad.S sid)
--     DynamicLog.def
--       { DynamicLog.ppCurrent         = DynamicLog.xmobarColor Colors.green   "" . DynamicLog.wrap "[" "]"
--       , DynamicLog.ppVisible         = DynamicLog.xmobarColor Colors.skyblue "" . DynamicLog.wrap "(" ")"
--       , DynamicLog.ppTitle           = DynamicLog.xmobarColor Colors.green   "" . DynamicLog.shorten 50
--       , DynamicLog.ppLayout          = DynamicLog.xmobarColor Colors.violet  ""
--       , DynamicLog.ppUrgent          = DynamicLog.xmobarColor Colors.red     "" . DynamicLog.wrap " " " "
--       , DynamicLog.ppHidden          = DynamicLog.xmobarColor Colors.blue    ""
--       , DynamicLog.ppWsSep           = DynamicLog.xmobarColor Colors.base02  "" " / "
--       , DynamicLog.ppSep             = DynamicLog.xmobarColor Colors.base02  "" "  |  "
--       , DynamicLog.ppHiddenNoWindows = const ""
--       , DynamicLog.ppOrder           = id -- \(ws:_:_:_) -> [ws]
--       -- , DynamicLog.ppOutput          = hPutStrLn handle --appendFile pipe . decodeString . (++ "\n")
--       , DynamicLog.ppOutput          = appendFile pipe . decodeString . (++ "\n")
--       -- , DynamicLog.ppOutput  = appendFile "$HOME/.local/tmp/xmonad-wss" . decodeString . (++ "\n")
--       , DynamicLog.ppSort            = fmap
--                                         (NamedScratchpad.namedScratchpadFilterOutWorkspace .)
--                                         (DynamicLog.ppSort DynamicLog.def)
--       }
