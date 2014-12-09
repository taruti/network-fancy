-----------------------------------------------------------------------------
--
-- Module      :  Network.Fancy.Internal
-- Copyright   :  Taru Karttunen <taruti@taruti.net>
-- License     :  BSD3
--
-- Maintainer  :  taruti@taruti.net
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Network.Fancy.Internal (
  Socket(..),
) where


import Foreign.C

newtype Socket = Socket CInt

