import Network.HTTP.Server hiding(Handler)
import Network.URL
import Network.Socket.Internal(SockAddr)
import System.IO
import Debug.Trace(trace)
import Text.Regex
import Control.Arrow(first)
import Network.URI
import qualified Data.ByteString as B
import Data.ByteString(ByteString)
import Data.ByteString.UTF8(fromString)
import MimeTypes

-----------------------------------------------------
loeb x = fmap ($ loeb x) x -- Where the magic happens
-----------------------------------------------------

type Handler = [String] -> IO (Response ByteString)

static :: FilePath -> Handler
static path _ =
  do content <- B.readFile path
     let contentLength = show $ B.length content
     return $ Response { rspCode = (2,0,0),
                         rspReason = "OK",
                         rspHeaders = [Header HdrContentType contentType,
                                       Header HdrContentLength contentLength],
                         rspBody = content }
  where contentType = mimeTypeForFilePath path

routes :: [(Regex, Handler)]
routes = map (first mkRegex) $ [
  ("/$", static "index.html"),
  ("/favicon\\.png", static "favicon.png")
  ]

page404 :: IO (Response ByteString)
page404 = return $ Response { rspCode = (4,0,4),
                              rspReason = "Not Found",
                              rspHeaders = [Header HdrContentType contentType,
                                            Header HdrContentLength contentLength],
                              rspBody = fromString body }
  where body = "Not Found"
        contentLength = show $ length body
        contentType = "text/plain"

httpd :: SockAddr -> URL -> Request a -> IO (Response ByteString)
httpd addr url req =
  case findRoute routes of
    Just responder -> responder
    Nothing -> page404
  where reqPath = uriPath $ rqURI req
        findRoute [] = Nothing
        findRoute ((regex, handler):routes) =
          case matchRegex regex reqPath of
            Just matches -> Just $ handler matches
            Nothing -> findRoute routes

main :: IO ()
main = server httpd

