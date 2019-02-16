
{-# LANGUAGE PackageImports #-}

module SHA256.Test
(
)
where

import "cryptohash" Crypto.Hash.SHA256
import Data.ByteString (pack, unpack, empty)
import Test.QuickCheck
import Tools

import SHA256
import SHA256.RepConv

sha256ref :: String -> String
sha256ref = w32x8ToHex . map w8x4ToW32 . group 4 . 
            map fromIntegral . unpack . hash . 
            pack . asciiToW8list

prop_equal :: [String] -> Bool
prop_equal strs = sha256 str == sha256ref str
  where
    str = concat strs