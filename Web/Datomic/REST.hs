{-# LANGUAGE OverloadedStrings #-}
module Web.Datomic.REST (
    ServerAddress,StorageName,DatabaseName,
    createDatabase,CreationError(..),
    transact,TransactionResult,TransactionError(..),
    q,Query,QueryInput,QueryResult,QueryError,
    InteractionError(..),UrlError(..)) where

import Network.URI (URI,parseRelativeReference,relativeTo)

import Network.HTTP (simpleHTTP,urlEncode,urlEncodeVars,mkRequest,Request(Request),getResponseCode,getResponseBody)
import Network.HTTP.Headers (mkHeader,insertHeader,HeaderName(HdrContentLength,HdrContentType,HdrAccept))
import Network.HTTP.Base (RequestMethod(GET,POST),ResponseCode)

import Network.TCP (HStream)
import Network.Stream (ConnError)

import qualified Data.EDN as EDN (TaggedValue)
import qualified Data.EDN.Parser as EDN (parseBSL)
import qualified Data.EDN.Encode as EDN (fromTagged)

import Control.Error (EitherT,runEitherT,noteT,hoistMaybe,hoistEither,left,fmapLT,scriptIO)
import Control.Monad (when)

import Data.Attoparsec.Lazy (eitherResult)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BC (length,pack)
import qualified Data.Text.Lazy as T (unpack)
import qualified Data.Text.Lazy.Builder as T (toLazyText)

-- DATA

-- STORAGE

type ServerAddress = URI

type StorageName = String

type DatabaseName = String

data CreationError = CreationInteractionError InteractionError
                   | CreationResponseCodeError (Int,Int,Int) ByteString
                   | CreationUrlError UrlError deriving Show

-- | Creates a Database with the given name on the given storage via the given server.
--   Returns `True` if the database was created and `False` if it existed.
createDatabase :: ServerAddress -> StorageName -> DatabaseName -> IO (Either CreationError Bool)
createDatabase serveraddress storagename databasename = runEitherT $ do

    datauri     <- noteT (CreationUrlError UrlComponentParseError) (hoistMaybe (parseRelativeReference "data/"))
    storageuri  <- noteT (CreationUrlError StorageNameError)       (hoistMaybe (parseRelativeReference (storagename++"/")))
    databaseuri <- noteT (CreationUrlError DatabaseNameError)      (hoistMaybe (parseRelativeReference (databasename++"/")))

    let request = Request uri POST [header1,header2,header3] body
        uri     = storageuri `relativeTo` datauri `relativeTo` serveraddress
        header1 = mkHeader HdrAccept        "application/edn"
        header2 = mkHeader HdrContentType   "application/x-www-form-urlencoded"
        header3 = mkHeader HdrContentLength (show (BC.length body))
        body    = BC.pack  ("db-name=" ++ urlEncode databasename)

    (code,body) <- webInteract request `onFailure` CreationInteractionError
    case code of
        (2,0,1) -> return True
        (2,0,0) -> return False
        _       -> left (CreationResponseCodeError code body)

-- DATABASE

--permalink :: ServerAddress -> StorageName -> DatabaseName -> Database
--permalink = undefined

--latest :: ServerAddress -> StorageName -> DatabaseName -> Database
--latest = undefined

--datoms :: ???

--entity :: Database -> EntityID -> EntityMap
--entity = undefined

--events :: ???

data TransactionError = TransactionInteractionError InteractionError |
                        TransactionResponseCodeError (Int,Int,Int) ByteString |
                        TransactionUrlError UrlError |
                        TransactionBodyParseError String deriving Show

type TransactionResult = EDN.TaggedValue

-- | Commit the given transaction to the given database on the given storage via the given server.
transact :: ServerAddress -> StorageName -> DatabaseName -> EDN.TaggedValue -> IO (Either TransactionError TransactionResult)
transact serveraddress storagename databasename transaction = runEitherT $ do

    datauri     <- noteT (TransactionUrlError UrlComponentParseError) (hoistMaybe (parseRelativeReference "data/"))
    storageuri  <- noteT (TransactionUrlError StorageNameError)       (hoistMaybe (parseRelativeReference (storagename++"/")))
    databaseuri <- noteT (TransactionUrlError DatabaseNameError)      (hoistMaybe (parseRelativeReference (databasename++"/")))

    let request = Request uri POST [header1,header2,header3] body
        uri     = databaseuri `relativeTo` storageuri `relativeTo` datauri `relativeTo` serveraddress
        header1 = mkHeader HdrAccept        "application/edn"
        header2 = mkHeader HdrContentType   "application/x-www-form-urlencoded"
        header3 = mkHeader HdrContentLength (show (BC.length body))
        body    = BC.pack  ("tx-data=" ++ urlEncode (ednToString transaction))

    (code,body) <- webInteract request `onFailure` TransactionInteractionError
    when (code /= (2,0,1)) (left (TransactionResponseCodeError code body))

    hoistEither (eitherResult (EDN.parseBSL body)) `onFailure` TransactionBodyParseError


-- API

type Query = EDN.TaggedValue

type QueryInput = EDN.TaggedValue

data QueryError = QueryInteractionError InteractionError |
                  QueryResponseCodeError (Int,Int,Int) ByteString |
                  QueryBodyParseError String |
                  UrlParseError deriving Show
    
type QueryResult = EDN.TaggedValue

-- | Queries the given server using the given queryinput and the given query.
q :: ServerAddress -> Query -> QueryInput -> IO (Either QueryError QueryResult)
q serveraddress query queryinput = runEitherT $ do

    let querystring = ednToString query
        inputstring = ednToString queryinput

    apiSlashQuery <- noteT UrlParseError (hoistMaybe (parseRelativeReference "api/query"))
    parameters    <- noteT UrlParseError (hoistMaybe (parseRelativeReference ("?" ++ urlEncodeVars
         [("q",querystring),
         ("args",inputstring),
         ("offset",""),
         ("limit","")])))

    let request = insertHeader HdrAccept "application/edn" (mkRequest GET uri)
        uri = parameters `relativeTo` apiSlashQuery `relativeTo` serveraddress

    (code,body) <- webInteract request `onFailure` QueryInteractionError
    when (code /= (2,0,0)) (left (QueryResponseCodeError code body))
    
    hoistEither (eitherResult (EDN.parseBSL body)) `onFailure` QueryBodyParseError

-- COMMON

data InteractionError = UnexpectedError String |
                        ConnectionError ConnError |
                        RetrievalError String deriving Show

type Body s = s

webInteract :: (HStream s) => Request s -> EitherT InteractionError IO (ResponseCode,Body s)
webInteract request = do
    result   <- scriptIO (simpleHTTP request) `onFailure` UnexpectedError
    response <- hoistEither result            `onFailure` ConnectionError
    code     <- scriptIO (getResponseCode (Right response)) `onFailure` RetrievalError
    body     <- scriptIO (getResponseBody (Right response)) `onFailure` RetrievalError
    return (code,body)

ednToString :: EDN.TaggedValue -> String
ednToString = T.unpack . T.toLazyText . EDN.fromTagged

onFailure :: Monad m => EitherT a m r -> (a -> b) -> EitherT b m r
onFailure = flip fmapLT

data UrlError = StorageNameError |
                DatabaseNameError |
                UrlComponentParseError deriving Show

