
{-# LANGUAGE PackageImports #-}

module SHA256.Test
(
)
where

import "cryptohash" Crypto.Hash.SHA256
import Data.ByteString (pack, unpack, empty)
import Test.QuickCheck

import SHA256
import SHA256.RepConv
import Tools

ref :: String -> String
ref = w32sToHex . map w8sToW32 . group 4 . map fromIntegral . unpack . hash . pack . asciiToW8s

prop_equal :: [String] -> Bool
prop_equal strs = sha256 str == ref str
  where
    str = concat strs




