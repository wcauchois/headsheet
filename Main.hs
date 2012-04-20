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
import Data.ByteString.UTF8(fromString, toString)
import MimeTypes
import Language.Haskell.Interpreter
import Network.HTTP.Types(parseSimpleQuery, SimpleQuery)
import Data.Maybe(fromJust)
import Data.IORef
import Data.Array
import System.IO.Unsafe(unsafePerformIO)
import Text.JSON(encode, JSON)
import Control.Monad(liftM)

-----------------------------------------------------
loeb :: Functor f => f (f a -> a) -> f a
loeb x = fmap ($ loeb x) x -- Where the magic happens
-----------------------------------------------------

type Handler a = [String] -> a -> Request ByteString -> IO (Response ByteString)
type Sheet = Array (Int, Int) String

okResponse :: String       -- Content type
           -> ByteString   -- Body
           -> Response ByteString
okResponse contentType body =
  Response { rspCode = (2,0,0),
             rspReason = "OK",
             rspHeaders = [Header HdrContentType contentType,
                           Header HdrContentLength (show $ B.length body)],
             rspBody = body }

jsonResponse :: JSON a => a -> Response ByteString
jsonResponse = okResponse "application/json" . fromString . encode

static :: FilePath -> Handler a
static path _ _ _ =
  do content <- B.readFile path
     return $ okResponse contentType content
  where contentType = mimeTypeForFilePath path

evalSheet :: Sheet -> [String]
evalSheet sheet = map show $ elems $ loeb sheet'
  where sheet' :: Array (Int, Int) (Array (Int, Int) Double -> Double)
        sheet' = fmap evalCell sheet
        evalCell :: String -> Array (Int, Int) Double -> Double
        evalCell expr =
          let expr' = "\\sheet -> " ++ expr
              result =
                unsafePerformIO $ runInterpreter $ do
                  setImports ["Prelude", "Data.Array"]
                  interpret expr' (as :: Array (Int, Int) Double -> Double)
          in case result of
               Left err -> error (show err)
               Right val -> val

queryGet :: SimpleQuery -> String -> String
queryGet query x = toString $ fromJust $ lookup (fromString x) query

updateHandler :: Handler (IORef Sheet)
updateHandler _ sheet req =
  do modifyIORef sheet (//[((row, col), formula)])
     readIORef sheet >>= print -- XXX
     return $ okResponse "text/plain" (fromString "Hi")
  where get = let query = parseSimpleQuery $ rqBody req
              in queryGet query
        row, col :: Int
        [row, col] = map (read . get) ["row", "col"]
        formula = get "formula"

evalHandler :: Handler (IORef Sheet)
evalHandler _ sheet req =
  do sheet' <- liftM evalSheet $ readIORef sheet
     return $ jsonResponse sheet'

formulaHandler :: Handler (IORef Sheet)
formulaHandler _ sheet req =
  liftM (jsonResponse . (!(row, col))) $ readIORef sheet
  where get = let query = parseSimpleQuery $ fromString $ tail $ uriQuery $ rqURI req
              in queryGet query
        row, col :: Int
        [row, col] = map (read . get) ["row", "col"]

routes :: [(Regex, Handler (IORef Sheet))]
routes = map (first mkRegex) $ [
  ("/$", static "index.html"),
  ("/favicon\\.png", static "favicon.png"),
  ("/update", updateHandler),
  ("/eval", evalHandler),
  ("/formula", formulaHandler)
  ]

page404 :: IO (Response ByteString)
page404 =
  let ok = okResponse "text/plain" (fromString "Not Found")
  in return $ ok { rspCode = (4,0,4),
                   rspReason = "Not Found" }

httpd :: IORef Sheet
      -> SockAddr
      -> URL
      -> Request ByteString
      -> IO (Response ByteString)
httpd sheet addr url req =
  case findRoute routes of
    Just responder -> responder sheet req
    Nothing -> page404
  where reqPath = uriPath $ rqURI req
        findRoute [] = Nothing
        findRoute ((regex, handler):routes) =
          case matchRegex regex reqPath of
            Just matches -> Just $ handler matches
            Nothing -> findRoute routes

main :: IO ()
main = do sheet <- newIORef (listArray ((0, 0), (4, 4)) $ repeat "0")
          server (httpd sheet)
