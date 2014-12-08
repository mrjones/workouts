{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)
--import qualified Data.Text.Lazy as T (Text)
-- sudo apt-get install libmysqlclient-dev
-- cabal install mysql-simple
import Database.MySQL.Simple
import Database.MySQL.Simple.Types as SQLT
-- cabal install happstack
import Happstack.Server (nullConf, simpleHTTP, toResponse, ok, ServerPart, Response)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

main:: IO()
main = do
  simpleHTTP nullConf $ helloPage

helloPage :: ServerPart Response
helloPage = do
    message <- liftIO $ myHead getMessage
    ok $ toResponse $ pageWithMessage message

pageWithMessage :: String -> H.Html
pageWithMessage msg =
  H.html $ do
    H.head $ do
      H.title $ "Hello, world!"
    H.body $ do
      H.toHtml msg

myHead :: IO [String] -> IO String
myHead xs = liftM Prelude.head xs

getMessage :: IO [String]
getMessage = do
  conn <- connect defaultConnectInfo
    { connectUser = "happstack"
    , connectPassword = "happstack"
    , connectDatabase = "happstack"
    }
  results <- query conn "SELECT Message FROM foo" ()
  return $ Prelude.map(\(Only r) -> r) results
                                                        

{-|
import Text.Blaze.Html5 (Html, toHtml)
import qualified Text.Blaze.Html5 as H
import Data.Text (Text)

homePage :: ServerPart Response
homePage = ok $ template "home page" $ do H.h1 (toHtml "Hello!")

template :: [Char] -> Html -> Response
template title body = toResponse $
                      H.html $ do
                        H.head $ do
                          H.title (toHtml title)
                        H.body $ do
                          body
-}
