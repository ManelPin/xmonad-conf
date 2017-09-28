{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable,
  TypeSynonymInstances, MultiParamTypeClasses #-}

----------------------------------------------------------------------------
-- |
-- Module       : Conf.Theme.Sizes
-- Copyright    : (c) maddy@na.ai
-- License      : MIT
--
-- Maintainer   : maddy@na.ai
-- Stability    : unstable
-- Portability  : unportable
--
----------------------------------------------------------------------------
module Conf.Theme.Sizes where

import qualified Data.Word as Word

gap:: Int
gap = 16

gapSmall :: Int
gapSmall = 8

gapBig :: Int
gapBig = 32

topbar :: Word.Word32
topbar = 16

tabbar :: Word.Word32
tabbar = 44

border :: Word.Word32
border = 0

prompt :: Word.Word32
prompt = 64

status :: Word.Word32
status = 16
