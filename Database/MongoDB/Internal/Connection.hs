
-- | This module defines a connection interface. It could be a regular
-- network connection, TLS connection, a mock or anything else.

module Database.MongoDB.Internal.Connection (
    Connection(..),
    readExactly,
    writeLazy,
) where

import Prelude hiding (read)
import Data.Monoid
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import qualified Data.ByteString.Lazy as Lazy.ByteString
import Control.Monad

-- | Abstract connection interface
data Connection = Connection {
    read :: IO ByteString,
    unread :: ByteString -> IO (),
    write :: ByteString -> IO (),
    flush :: IO (),
    close :: IO ()}

readExactly :: Connection -> Int -> IO Lazy.ByteString
-- ^ Read specified number of bytes
readExactly conn count = go mempty count
  where
  go acc n = do
    -- read until get enough bytes
    chunk <- read conn
    let len = ByteString.length chunk
    if len >= n
      then do
        let (res, rest) = ByteString.splitAt n chunk
        unless (ByteString.null rest) $
          unread conn rest
        return (acc <> Lazy.ByteString.fromStrict res)
      else go (acc <> Lazy.ByteString.fromStrict chunk) (n - len)

writeLazy :: Connection -> Lazy.ByteString -> IO ()
writeLazy conn = mapM_ (write conn) . Lazy.ByteString.toChunks
