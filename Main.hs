import Network.HTTP.Server
import Network.URL
import Network.Socket.Internal

myHandler :: SockAddr -> URL -> Request a -> IO (Response String)
myHandler addr url req = return $ Response { rspCode = (2,0,0),
                                             rspReason = "OK",
                                             rspHeaders = [Header HdrContentType "text/plain", Header HdrContentLength (show $ length x)],
                                             rspBody =  x } where x = "Hi!"

main :: IO ()
main = server myHandler
