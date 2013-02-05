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

type ServerAddress = URI

data StorageName = StorageName String

data DatabaseName = DatabaseName String

data Database = Database

data Transaction = Transaction

data TransactionError = TransactionError

data TransactionResult = TransactionResult

transact :: ServerAddress -> StorageName -> Transaction -> IO (Either TransactionError TransactionResult)
transact = undefined

-- API

type Query = EDN.TaggedValue

type QueryInput = EDN.TaggedValue

data QueryError = ConnectionError ConnError |
                  RetrievalError String |
                  ResponseCodeError (Int,Int,Int) |
                  BodyParseError String |
                  UrlParseError deriving Show

type QueryResult = EDN.TaggedValue

q :: ServerAddress -> Query -> QueryInput -> IO (Either QueryError QueryResult)
q serveraddress query queryinput = runEitherT $ do
    let ednToString = T.unpack . T.toLazyText . EDN.fromTagged
        querystring = ednToString query
        inputstring = ednToString queryinput
    apiSlashQuery <- noteT UrlParseError (hoistMaybe (parseRelativeReference "api/query"))
    parameters <- noteT UrlParseError (hoistMaybe (parseRelativeReference ("?" ++ urlEncodeVars
         [("q",querystring),
         ("args",inputstring),
         ("offset",""),
         ("limit","")])))
    let req = insertHeader HdrAccept "application/edn" (mkRequest GET uri)
        uri = parameters `relativeTo` apiSlashQuery `relativeTo` serveraddress
    response <- lift (simpleHTTP req)
    result <- hoistEither response `onFailure` ConnectionError
    code <- scriptIO (getResponseCode (Right result)) `onFailure` RetrievalError
    when (code /= (2,0,0)) (left (ResponseCodeError code))
    --TODO extrace more information if response code indicates an error
    body <- scriptIO (getResponseBody (Right result)) `onFailure` RetrievalError
    hoistEither (eitherResult (EDN.parseBSL body)) `onFailure` BodyParseError

onFailure :: Monad m => EitherT a m r -> (a -> b) -> EitherT b m r
onFailure = flip fmapLT
    


testAddress :: URI
testAddress = fromJust (parseURI "http://127.0.0.1:9834")

Just testQuery = maybeResult (EDN.parseS "[:find ?e ?v :in $ :where [?e :db/doc ?v]]")

Just testQueryInput = maybeResult (EDN.parseS "[{:db/alias \"whatup/tst\"}]")


main :: IO ()
main = q testAddress testQuery testQueryInput >>= print






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


