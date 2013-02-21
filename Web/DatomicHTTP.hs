module DatomicHTTP where

import Network.URI
import Network.HTTP
import Network.Stream
import qualified Data.EDN as EDN
import qualified Data.EDN.Parser as EDN

import Data.Maybe

import Control.Error

import Control.Monad.Trans

import Data.Attoparsec.Lazy

import Data.ByteString.Lazy

import qualified Data.ByteString.Lazy.Char8 as BC

import Control.Monad

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Builder as T

import qualified Data.EDN.Encode as EDN

-- DATA

-- STORAGE

createDatabase :: Connection -> DatabaseName -> IO Bool
createDatabase = undefined

-- DATABASE

permalink :: Connection -> DatabaseName -> Database
permalink = undefined

latest :: Connection -> DatabaseName -> Database
latest = undefined

--wtf datoms :: 

data EntityID = EntityID

data EntityMap = EntityMap

entity :: Database -> EntityID -> EntityMap
entity = undefined

--wtf events ::

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

--

type ServerAddress = URI

type StorageName = String

type DatabaseName = String

data Database = Database

type Transaction = EDN.TaggedValue

data TransactionError = TransactionInteractionError InteractionError |
                        TransactionResponseCodeError (Int,Int,Int) ByteString |
                        StorageNameError |
                        DatabaseNameError |
                        UrlComponentParseError |
                        TransactionBodyParseError String deriving Show

type TransactionResult = EDN.TaggedValue

transact :: ServerAddress -> StorageName -> DatabaseName -> Transaction -> IO (Either TransactionError TransactionResult)
transact serveraddress storagename databasename transaction = runEitherT $ do

    datauri     <- noteT UrlComponentParseError (hoistMaybe (parseRelativeReference "data/"))
    storageuri  <- noteT StorageNameError       (hoistMaybe (parseRelativeReference (storagename++"/")))
    databaseuri <- noteT DatabaseNameError      (hoistMaybe (parseRelativeReference (databasename++"/")))

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


-- TESTS

testAddress :: URI
testAddress = fromJust (parseURI "http://127.0.0.1:9834")

Just testQuery = maybeResult (EDN.parseS "[:find ?e ?v :in $ :where [?e :db/doc ?v]]")
Just testQuery2 = maybeResult (EDN.parseS "[:find ?e :in $ :where [?e :db/doc \"I'm crazy!\"]]")

Just testQueryInput = maybeResult (EDN.parseS "[{:db/alias \"whatup/tst\"}]")

test = q testAddress testQuery testQueryInput >>= print

Just testTransaction = maybeResult (EDN.parseS "[{:db/id #db/id[:db.part/user] :db/doc \"I'm crazy!\"}]")

main :: IO ()
main = transact testAddress "whatup" "tst" testTransaction >>= print >> q testAddress testQuery2 testQueryInput >>= print






{-

data Location = Location URI String String

type Transaction = String

type Query = String




transact :: Location -> Transaction -> IO (Result (Response String))
transact (Location server storage database) transaction = simpleHTTP request where
    request = insertHeader HdrAccept "*/*" $ postRequestWithBody (show uri) "application/x-www-form-urlencoded" ("tx-data="++transaction)
    uri = subURI `relativeTo` server
    Just subURI = parseRelativeReference ("data/" ++ urlEncode storage ++ "/" ++ urlEncode database ++ "/")

query :: Location -> Query -> IO String
query (Location server storage database) querie = print uri >> simpleHTTP req >>= getResponseBody where
    uri = parameters `relativeTo` apiSlashQuery `relativeTo` server
    Just parameters = parseRelativeReference ("?" ++ urlEncodeVars
        [("q",querie),
         ("args","[{:db/alias \""++urlEncode storage++"/"++urlEncode database ++ "\"}]"),
         ("offset",""),
         ("limit","")])
    Just apiSlashQuery = parseRelativeReference "api/query"
    hdr = mkHeader HdrAccept "application/edn"
    req = Request uri GET [hdr] ""

server = fromJust (parseURI "http://127.0.0.1:9834")
storage = "whatup"
database = "tst"

test1 = query (Location server storage database) "[:find ?e ?v :in $ :where [?e :db/doc ?v]]" >>= print

test2 = transact (Location server storage database) "[{:db/id #db/id[:db.part/user] :db/doc \"I'm new!\"}]" >>= print

main :: IO ()
main = print "hello"
    
-}


