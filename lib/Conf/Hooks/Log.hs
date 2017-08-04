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
  -- , shutdownHandler
  ) where

import Prelude hiding (log)

import Control.Exception
        ( throw
        , SomeException(SomeException)
        )
import Control.Monad
        ( replicateM
        , forM_
        , forM
        , foldM
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

import qualified GHC.IO.Handle

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
        , hPutStrLn
        , spawnPipe
        )

type Pipe         = FilePath
-- type Handle       = GHC.IO.Handle.Handle
data XMobarOption = XMobarOption XMonad.ScreenId Pipe --Handle

log bars
 = do
  FadeWindows.fadeWindowsLogHook Hooks.Fade.fade
  EwmhDesktops.ewmhDesktopsLogHook
  -- forM_ bars $ \(XMobarOption sid pipe handle) -> do
  forM_ bars $ \(XMobarOption sid pipe) -> do
    -- let ppFoc = ppFocus      foc sid
    let pp = ppWorkspaces pipe sid
    -- DynamicLog.dynamicLogWithPP ppFoc
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


-- ppWorkspaces :: GHC.IO.Handle.Handle -> XMonad.ScreenId -> DynamicLog.PP
ppWorkspaces :: FilePath -> XMonad.ScreenId -> DynamicLog.PP
-- ppWorkspaces handle (XMonad.S sid) =
ppWorkspaces pipe (XMonad.S sid) =
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
                    -- handle <- spawnPipe (xmobarCommand sid pipe)
                    return $ XMobarOption sid pipe --handle
    -- mapM_ (unsafeSpawn . xmobarCommand) barOptions
    spawnPipe $ initCommand barOptions
    return barOptions

-- killBars :: [XMobarOption] -> IO ()
-- killBars bars
--   = do
--     forM_ bars $ \(XMobarOption sid pipe handle) -> do
--       hClose handle
--
-- shutdownHandler :: [XMobarOption] -> SomeException -> IO ()
-- shutdownHandler bars e
--   = do
--     killBars bars
--     unsafeSpawn "dunstify -u critical 'XMonad' 'shutdownHandler called'"
--     throw e

initCommand :: [XMobarOption] -> String
initCommand xs = "$HOME/.xmonad/initbars.sh " ++ (unwords $ xmobarCommands xs)
  -- where concatx x1 x2 = x1 ++ " " ++ x2

xmobarCommands :: [XMobarOption] -> [String]
xmobarCommands = map xmobarCommand'

-- xmobarCommand :: XMonad.ScreenId -> Pipe -> String
-- xmobarCommand (XMonad.S sid) pipe =
--   "xmobar -x " ++ show sid ++ " -C '[Run PipeReader \"" ++ (show sid) ++ ":" ++ pipe ++ "\" \"pipe\"]' $HOME/.xmonad/xmobar.conf"

xmobarCommand' :: XMobarOption -> String
xmobarCommand' (XMobarOption (XMonad.S sid) pipe) =
  show sid ++ " " ++ pipe
  -- "xmobar -x " ++ show sid ++ " -C \'[Run PipeReader \\\"" ++ (show sid) ++ ":" ++ pipe ++ "\\\" \\\"pipe\\\"]\' $HOME/.xmonad/xmobar.conf &"
  -- "xmobar -x " ++ show sid ++ " -C '[Run PipeReader \"" ++ (show sid) ++ ":" ++ pipe ++ "\" \"pipe\"]' $HOME/.xmonad/xmobar.conf"

-- xmobarCommand :: XMobarOption -> String
-- xmobarCommand (XMobarOption (XMonad.S sid) pipe) =
--   "xmobar -x " ++ show sid ++ " -C '[Run PipeReader \"" ++ (show sid) ++ ":" ++ pipe ++ "\" \"pipe\"]' $HOME/.xmonad/xmobar.conf"
--
-- TODO: Move out of module
getTempFifo :: String -> IO FilePath
getTempFifo prefix = do
  tmpDir <- getTemporaryDirectory
  (tmpFile, h) <- openTempFile tmpDir prefix
  hClose h
  removeFile tmpFile
  createNamedPipe tmpFile $ unionFileModes ownerReadMode ownerWriteMode
  -- appendFile tmpFile "foo"
  return tmpFile
