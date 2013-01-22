module DatomicHTTP where

import Network.HTTP (simpleHTTP,getResponseBody)
import Network.HTTP.Base (Request(Request),RequestMethod(GET))
import Network.HTTP.Headers (Header(Header),HeaderName(HdrAccept))
import Network.URI (parseURI)
import Data.Maybe (fromJust)


main :: IO ()
main = do
    let uri = fromJust (parseURI "http://127.0.0.1:9834/api/query?q=%5B%3Afind+%3Fe+%3Fv+%3Ain+%24+%3Awhere+%5B%3Fe+%3Adb%2Fdoc+%3Fv%5D%5D&args=%5B%7B%3Adb%2Falias+%22whatup%2Ftst%22%7D%5D&offset=&limit=")
        hdr = Header HdrAccept "application/edn"
        req = Request uri GET [hdr] ""
    simpleHTTP req >>= getResponseBody >>= print


