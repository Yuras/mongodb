{-| Low-level messaging between this client and the MongoDB server, see Mongo Wire Protocol (<http://www.mongodb.org/display/DOCS/Mongo+Wire+Protocol>).

This module is not intended for direct use. Use the high-level interface at "Database.MongoDB.Query" and "Database.MongoDB.Connection" instead. -}

{-# LANGUAGE RecordWildCards, StandaloneDeriving, OverloadedStrings, FlexibleContexts, TupleSections, TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}

module Database.MongoDB.Internal.Protocol (
	-- * Network
	Network', ANetwork', Internet(..),
	-- * Pipe
	Pipe, send, call,
	-- * Message
	FullCollection,
	-- ** Notice
	Notice(..), UpdateOption(..), DeleteOption(..), CursorId,
	-- ** Request
	Request(..), QueryOption(..),
	-- ** Reply
	Reply(..), ResponseFlag(..),
	-- * Authentication
	Username, Password, Nonce, pwHash, pwKey
) where

import Prelude as X
import Control.Applicative ((<$>))
import Control.Arrow ((***))
import System.IO (Handle)
import Data.ByteString.Lazy as B (length, hPut)
import qualified Control.Pipeline as P
import Data.Bson (Document, UString)
import Data.Bson.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Int
import Data.Bits
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Data.Digest.OpenSSL.MD5 (md5sum)
import Data.UString as U (pack, append, toByteString)
import System.IO.Error as E (try)
import Control.Monad.Error
import Control.Monad.Util (whenJust)
import Network.Abstract (IOE, ANetwork, Network(..), Connection(Connection))
import Network (connectTo)
import System.IO (hFlush, hClose)
import Database.MongoDB.Internal.Util (hGetN, bitOr)

-- * Network

-- Network -> Server -> (Sink, Source)
-- (Sink, Source) -> Pipeline

type Message = ([Notice], Maybe (Request, RequestId))
-- ^ Write notice(s), write notice(s) with getLastError request, or just query request
-- Note, that requestId will be out of order because request ids will be generated for notices, after the request id supplied was generated. This is ok because the mongo server does not care about order they are just used as unique identifiers.

type Response = (ResponseTo, Reply)

class (Network n Message Response) => Network' n
instance (Network n Message Response) => Network' n

type ANetwork' = ANetwork Message Response

data Internet = Internet
-- ^ Normal Network instance, i.e. no logging or replay

-- | Connect to server. Write messages and receive replies; not thread-safe!
instance Network Internet Message Response where
	connect _ (hostname, portid) = ErrorT . E.try $ do
		handle <- connectTo hostname portid
		return $ Connection (sink handle) (source handle) (hClose handle)
	 where
		sink h (notices, mRequest) = ErrorT . E.try $ do
			forM_ notices $ \n -> writeReq h . (Left n,) =<< genRequestId
			whenJust mRequest $ writeReq h . (Right *** id)
			hFlush h
		source h = ErrorT . E.try $ readResp h
				
writeReq :: Handle -> (Either Notice Request, RequestId) -> IO ()
writeReq handle (e, requestId) = do
	hPut handle lenBytes
	hPut handle bytes
 where
	bytes = runPut $ (either putNotice putRequest e) requestId
	lenBytes = encodeSize . toEnum . fromEnum $ B.length bytes
	encodeSize = runPut . putInt32 . (+ 4)

readResp :: Handle -> IO (ResponseTo, Reply)
readResp handle = do
	len <- fromEnum . decodeSize <$> hGetN handle 4
	runGet getReply <$> hGetN handle len
 where
	decodeSize = subtract 4 . runGet getInt32

-- * Pipe

type Pipe = P.Pipeline Message Response
-- ^ Thread-safe TCP connection with pipelined requests

send :: Pipe -> [Notice] -> IOE ()
-- ^ Send notices as a contiguous batch to server with no reply. Throw IOError if connection fails.
send pipe notices = P.send pipe (notices, Nothing)

call :: Pipe -> [Notice] -> Request -> IOE (IOE Reply)
-- ^ Send notices and request as a contiguous batch to server and return reply promise, which will block when invoked until reply arrives. This call and resulting promise will throw IOError if connection fails.
call pipe notices request = do
	requestId <- genRequestId
	promise <- P.call pipe (notices, Just (request, requestId))
	return $ check requestId <$> promise
 where
	check requestId (responseTo, reply) = if requestId == responseTo then reply else
		error $ "expected response id (" ++ show responseTo ++ ") to match request id (" ++ show requestId ++ ")"

-- * Messages

type FullCollection = UString
-- ^ Database name and collection name with period (.) in between. Eg. \"myDb.myCollection\"

-- ** Header

type Opcode = Int32

type RequestId = Int32
-- ^ A fresh request id is generated for every message

type ResponseTo = RequestId

genRequestId :: (MonadIO m) => m RequestId
-- ^ Generate fresh request id
genRequestId = liftIO $ atomicModifyIORef counter $ \n -> (n + 1, n) where
	counter :: IORef RequestId
	counter = unsafePerformIO (newIORef 0)
	{-# NOINLINE counter #-}

-- *** Binary format

putHeader :: Opcode -> RequestId -> Put
-- ^ Note, does not write message length (first int32), assumes caller will write it
putHeader opcode requestId = do
	putInt32 requestId
	putInt32 0
	putInt32 opcode

getHeader :: Get (Opcode, ResponseTo)
-- ^ Note, does not read message length (first int32), assumes it was already read
getHeader = do
	_requestId <- getInt32
	responseTo <- getInt32
	opcode <- getInt32
	return (opcode, responseTo)

-- ** Notice

-- | A notice is a message that is sent with no reply
data Notice =
	  Insert {
	  	iFullCollection :: FullCollection,
	  	iDocuments :: [Document]}
	| Update {
		uFullCollection :: FullCollection,
		uOptions :: [UpdateOption],
		uSelector :: Document,
		uUpdater :: Document}
	| Delete {
		dFullCollection :: FullCollection,
		dOptions :: [DeleteOption],
		dSelector :: Document}
	| KillCursors {
		kCursorIds :: [CursorId]}
	deriving (Show, Eq)

data UpdateOption =
	  Upsert  -- ^ If set, the database will insert the supplied object into the collection if no matching document is found
	| MultiUpdate  -- ^ If set, the database will update all matching objects in the collection. Otherwise only updates first matching doc
	deriving (Show, Eq)

data DeleteOption = SingleRemove  -- ^ If set, the database will remove only the first matching document in the collection. Otherwise all matching documents will be removed
	deriving (Show, Eq)

type CursorId = Int64

-- *** Binary format

nOpcode :: Notice -> Opcode
nOpcode Update{} = 2001
nOpcode Insert{} = 2002
nOpcode Delete{} = 2006
nOpcode KillCursors{} = 2007

putNotice :: Notice -> RequestId -> Put
putNotice notice requestId = do
	putHeader (nOpcode notice) requestId
	putInt32 0
	case notice of
		Insert{..} -> do
			putCString iFullCollection
			mapM_ putDocument iDocuments
		Update{..} -> do
			putCString uFullCollection
			putInt32 (uBits uOptions)
			putDocument uSelector
			putDocument uUpdater
		Delete{..} -> do
			putCString dFullCollection
			putInt32 (dBits dOptions)
			putDocument dSelector
		KillCursors{..} -> do
			putInt32 $ toEnum (X.length kCursorIds)
			mapM_ putInt64 kCursorIds

uBit :: UpdateOption -> Int32
uBit Upsert = bit 0
uBit MultiUpdate = bit 1

uBits :: [UpdateOption] -> Int32
uBits = bitOr . map uBit

dBit :: DeleteOption -> Int32
dBit SingleRemove = bit 0

dBits :: [DeleteOption] -> Int32
dBits = bitOr . map dBit

-- ** Request

-- | A request is a message that is sent with a 'Reply' returned
data Request =
	  Query {
		qOptions :: [QueryOption],
		qFullCollection :: FullCollection,
		qSkip :: Int32,  -- ^ Number of initial matching documents to skip
		qBatchSize :: Int32,  -- ^ The number of document to return in each batch response from the server. 0 means use Mongo default. Negative means close cursor after first batch and use absolute value as batch size.
		qSelector :: Document,  -- ^ \[\] = return all documents in collection
		qProjector :: Document  -- ^ \[\] = return whole document
	} | GetMore {
		gFullCollection :: FullCollection,
		gBatchSize :: Int32,
		gCursorId :: CursorId}
	deriving (Show, Eq)

data QueryOption =
	  TailableCursor  -- ^ Tailable means cursor is not closed when the last data is retrieved. Rather, the cursor marks the final object's position. You can resume using the cursor later, from where it was located, if more data were received. Like any "latent cursor", the cursor may become invalid at some point – for example if the final object it references were deleted. Thus, you should be prepared to requery on CursorNotFound exception.
	| SlaveOK  -- ^ Allow query of replica slave. Normally these return an error except for namespace "local".
	| NoCursorTimeout  -- The server normally times out idle cursors after an inactivity period (10 minutes) to prevent excess memory use. Set this option to prevent that.
	| AwaitData  -- ^ Use with TailableCursor. If we are at the end of the data, block for a while rather than returning no data. After a timeout period, we do return as normal.
--	| Exhaust  -- ^ Stream the data down full blast in multiple "more" packages, on the assumption that the client will fully read all data queried. Faster when you are pulling a lot of data and know you want to pull it all down. Note: the client is not allowed to not read all the data unless it closes the connection.
	deriving (Show, Eq)

-- *** Binary format

qOpcode :: Request -> Opcode
qOpcode Query{} = 2004
qOpcode GetMore{} = 2005

putRequest :: Request -> RequestId -> Put
putRequest request requestId = do
	putHeader (qOpcode request) requestId
	case request of
		Query{..} -> do
			putInt32 (qBits qOptions)
			putCString qFullCollection
			putInt32 qSkip
			putInt32 qBatchSize
			putDocument qSelector
			unless (null qProjector) (putDocument qProjector)
		GetMore{..} -> do
			putInt32 0
			putCString gFullCollection
			putInt32 gBatchSize
			putInt64 gCursorId

qBit :: QueryOption -> Int32
qBit TailableCursor = bit 1
qBit SlaveOK = bit 2
qBit NoCursorTimeout = bit 4
qBit AwaitData = bit 5
--qBit Exhaust = bit 6

qBits :: [QueryOption] -> Int32
qBits = bitOr . map qBit

-- ** Reply

-- | A reply is a message received in response to a 'Request'
data Reply = Reply {
	rResponseFlags :: [ResponseFlag],
	rCursorId :: CursorId,  -- ^ 0 = cursor finished
	rStartingFrom :: Int32,
	rDocuments :: [Document]
	} deriving (Show, Eq)

data ResponseFlag =
	  CursorNotFound  -- ^ Set when getMore is called but the cursor id is not valid at the server. Returned with zero results.
	| QueryError  -- ^ Query error. Returned with one document containing an "$err" field holding the error message.
	| AwaitCapable  -- ^ For backward compatability: Set when the server supports the AwaitData query option. if it doesn't, a replica slave client should sleep a little between getMore's
	deriving (Show, Eq, Enum)

-- * Binary format

replyOpcode :: Opcode
replyOpcode = 1

getReply :: Get (ResponseTo, Reply)
getReply = do
	(opcode, responseTo) <- getHeader
	unless (opcode == replyOpcode) $ fail $ "expected reply opcode (1) but got " ++ show opcode
	rResponseFlags <-  rFlags <$> getInt32
	rCursorId <- getInt64
	rStartingFrom <- getInt32
	numDocs <- fromIntegral <$> getInt32
	rDocuments <- replicateM numDocs getDocument
	return (responseTo, Reply{..})

rFlags :: Int32 -> [ResponseFlag]
rFlags bits = filter (testBit bits . rBit) [CursorNotFound ..]

rBit :: ResponseFlag -> Int
rBit CursorNotFound = 0
rBit QueryError = 1
rBit AwaitCapable = 3

-- * Authentication

type Username = UString
type Password = UString
type Nonce = UString

pwHash :: Username -> Password -> UString
pwHash u p = pack . md5sum . toByteString $ u `U.append` ":mongo:" `U.append` p

pwKey :: Nonce -> Username -> Password -> UString
pwKey n u p = pack . md5sum . toByteString . U.append n . U.append u $ pwHash u p
