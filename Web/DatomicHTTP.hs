module DatomicHTTP where

import Network.HTTP (simpleHTTP,getResponseBody,postRequestWithBody,getRequest)
import Network.HTTP.Base (Request(Request),RequestMethod(GET,POST),Response,urlEncodeVars,urlEncode,setRequestBody)
import Network.HTTP.Headers (Header,HeaderName(HdrAccept,HdrContentType),mkHeader,insertHeader)
import Network.URI (parseURI,URI,relativeTo,parseRelativeReference)
import Network.Stream (Result)
import Data.Maybe (fromJust)

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
    


