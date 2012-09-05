{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Application.Static

myWaiApp :: Application
myWaiApp = staticApp $ defaultFileServerSettings "."

main :: IO ()
main = run 9191 myWaiApp
