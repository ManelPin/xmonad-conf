{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable,
  TypeSynonymInstances, MultiParamTypeClasses #-}

----------------------------------------------------------------------------
-- |
-- Module       : Conf.Workspaces
-- Copyright    : (c) maddy@na.ai
-- License      : MIT
--
-- Maintainer   : maddy@na.ai
-- Stability    : unstable
-- Portability  : unportable
--
----------------------------------------------------------------------------
module Conf.Workspaces
  ( workspaces
  )
where

import Data.List
        ( transpose
        )

screenMain :: [String]
screenMain = [ "Web", "Dev", "Dev", "Dev", "Misc" ]

screenVert :: [String]
screenVert = [ "Soc", "Dev", "Dev", "Dev", "Misc" ]

workspaces :: Int -> [String]
workspaces sc = concat . transpose $ zipWith fmt [0..] (wss sc)

fmt :: Int -> [String] -> [String]
fmt sid ws = zipWith (\wsname wsnum -> show sid ++ "_" ++ show wsnum ++ ":" ++ wsname) ws [1..]

wss :: Int -> [[String]]
wss sc
  | sc == 1 = [screenMain]
  | sc == 2 = [screenMain, screenMain]
  | sc == 3 = [screenMain, screenVert, screenMain]
  | otherwise = take sc $ repeat screenMain

