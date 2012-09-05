{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.Wai.Handler.Webkit
import Network.Wai.Application.Static

myWaiApp :: Application
myWaiApp = staticApp $ defaultFileServerSettings "."

main :: IO ()
main = run "MyWaiApp" myWaiApp
