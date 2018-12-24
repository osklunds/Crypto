
module SHA256.SHA256
(
)
where

import Data.Word

import SHA256.Pad
import SHA256.MerkleDamgard
import SHA256.Constants
import SHA256.Compression
import SHA256.MessageSchedule
import Tools


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
    cv = Comp [a, b, c, d, e, f, g, h]
    (Comp [a', b', c', d', e', f', g', h']) = comp cv k (wc ms)

inner :: [MDMes] -> MDTag
inner = merkleDamgard mdComp mdIV

outer :: [Word32] -> [Word32]
outer ms = hashed
  where
    padded = pad ms
    chunks = group 16 padded
    chunks' = map MDMes chunks
    (MDTag hashed) = inner chunks'






