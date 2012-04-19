module MimeTypes(mimeTypeForFilePath) where
import Data.Maybe(fromJust)
import System.FilePath.Posix(takeExtension)

mimeTypes :: [(String, String)]
mimeTypes = [(".png", "image/png"),
             (".txt", "text/plain"),
             (".html", "text/html")
            ]

mimeTypeForFilePath :: String -> String
mimeTypeForFilePath = fromJust . (`lookup` mimeTypes) . takeExtension

