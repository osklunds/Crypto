
module SHA256.SHA256
(
)
where

import Data.Word
import Data.Bits
import Numeric
import Data.Char
import Test.QuickCheck hiding ((.&.))

import Math.Prime
import Math.Common
import Math.BigInt


-- Merkle-Damgård. Given a compression function
-- taking a tag and a message, this gives
-- a function taking arbitrarily long list
-- of messages, using the compression function
-- and making it into a tag
-- Need to supply initial tag/iv.
merkleDamgard :: (t -> m -> t) -> t -> ([m] -> t)
merkleDamgard _ t []     = t
merkleDamgard h t (m:ms) = merkleDamgard h (h t m) ms


-- The type of the tag used in the MD function
newtype MDTag = MDTag [Word32] -- length = 8
              deriving Show

-- The type of the message used in the MD function
newtype MDMes = MDMes [Word32] -- length = 16
              deriving Show




mdIV :: MDTag
mdIV = MDTag $ map k [0..7]

mdComp :: MDTag -> MDMes -> MDTag
mdComp (MDTag [a,b,c,d,e,f,g,h]) (MDMes ms) =
  MDTag [a',b',c',d',e',f',g',h']
  where
    cv = Comp a b c d e f g h
    (Comp a' b' c' d' e' f' g' h') = comp cv k (wc ms)

inner :: [MDMes] -> MDTag
inner = merkleDamgard mdComp mdIV





