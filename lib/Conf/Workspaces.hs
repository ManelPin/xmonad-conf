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

tags :: [String]
tags = [ "A", "B", "C", "D", "E", "F" ]

workspaces :: Int -> [String]
workspaces sc = concat . transpose $ zipWith fmt [0..] (wss sc)

fmt :: Int -> [String] -> [String]
fmt sid ws = zipWith (\wsname wsnum -> show sid ++ "_" ++ show wsnum ++ ":" ++ wsname) ws [1..]

wss :: Int -> [[String]]
wss sc = take sc $ repeat tags
