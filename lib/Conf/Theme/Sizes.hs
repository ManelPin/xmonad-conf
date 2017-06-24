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
gap = 10

gapSmall :: Int
gapSmall = gap `quot` 2

gapBig :: Int
gapBig = gap * 2

topbar :: Word.Word32
topbar = 10

border :: Word.Word32
border = 0

prompt :: Word.Word32
prompt = 20

status :: Word.Word32
status = 20
