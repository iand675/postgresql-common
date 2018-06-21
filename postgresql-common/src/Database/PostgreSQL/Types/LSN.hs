{-# LANGUAGE BangPatterns #-}
module Database.PostgreSQL.Types.LSN
    ( LSN(..)
    -- * Utilities
    , encodeLSN
    , decodeLSN
    , lsnParser
    ) where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Builder as Builder
import Data.ByteString.Lazy.Builder.ASCII (word32Hex, word32HexFixed)
import Data.Monoid ((<>))
import Data.Word
import Data.Attoparsec.ByteString.Char8

data LSN = LSN
  { filepart :: !Word32 -- ^ Filepart
  , offset :: !Word32 -- ^ Offset
  }
  deriving (Show, Eq)

instance Ord LSN where
  compare (LSN l0 r0) (LSN l1 r1)
    | l0 > l1 || (l0 == l1 && r0 > r1) = GT
    | l0 < l1 || (l0 == l1 && r0 < r1) = LT
    | otherwise = EQ

lsnParser :: Parser LSN
lsnParser = LSN <$> (hexadecimal <* char '/') <*> hexadecimal

decodeLSN :: ByteString -> Either String LSN
decodeLSN = parseOnly lsnParser

encodeLSN :: LSN -> ByteString
encodeLSN (LSN filepart off) = BL.toStrict $ Builder.toLazyByteString (word32Hex filepart <> Builder.char7 '/' <> word32Hex off)

-- instance ToField LSN where

-- instance FromField LSN where
