{-# LANGUAGE OverloadedStrings #-}
import qualified Network.Wai as W
import qualified Network.HTTP.Types as H
import qualified Network.Wai.Handler.Warp as WP
import qualified Data.ByteString.Lazy as L

header :: H.ResponseHeaders -- [(HeaderName, ByteString)]
header = [("Content-Type", "text/plain")]

myWaiApp :: W.Application -- Request -> ResourceT IO Response
myWaiApp req | W.requestMethod req /= "GET" = return r
  where r = W.responseLBS H.status405 header "Only GET is supported"
myWaiApp req = return r
  where
    s = "Path:" `L.append` L.fromChunks [W.rawPathInfo req]
    r = W.responseLBS H.status200 header s

main :: IO ()
main = WP.run 9191 myWaiApp
