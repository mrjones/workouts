{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (liftM, msum)
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
-- sudo apt-get install libmysqlclient-dev
-- cabal install mysql-simple
import Database.MySQL.Simple
import Database.MySQL.Simple.Types as SQLT
-- cabal install happstack
import Happstack.Server (dir, nullConf, simpleHTTP, toResponse, ok, ServerPart, Response, ServerPartT)
import Text.Blaze.Html5 as H
--import Text.Blaze.Html5.Attributes as A

main:: IO()
main = do
  simpleHTTP nullConf $ allPages

allPages :: ServerPartT IO Response
allPages = msum
           [ mkTablePage
           , helloPage
           ]

mkTablePage :: ServerPartT IO Response
mkTablePage = dir "admin" $ dir "mktable" $ do
  i <- liftIO mkTable
  ok (toResponse (mkTableHtml i))

mkTableHtml :: Int64 -> H.Html
mkTableHtml _ =
  H.html $ do
    H.head $ do
      H.title $ "mktable page"
    H.body "mktable"

helloPage :: ServerPartT IO Response
helloPage = do
    message <- liftIoMyHead
    helloPage2 message

helloPage2 :: String -> ServerPartT IO Response
helloPage2 msg = ok (toResponse (pageWithMessage msg))

pageWithMessage :: String -> H.Html
pageWithMessage msg =
  H.html $ do
    H.head $ do
      H.title $ "Hello, world!"
    H.body $ do
      H.toHtml msg

liftIoMyHead :: ServerPartT IO String
liftIoMyHead = liftIO $ myHead getMessage

myHead :: IO [String] -> IO String
myHead xs = liftM Prelude.head xs

dbConnect = connect defaultConnectInfo
    { connectUser = "happstack"
    , connectPassword = "happstack"
    , connectDatabase = "happstack"
    }

getMessage :: IO [String]
getMessage = do
  conn <- dbConnect
  results <- query conn "SELECT Message FROM foo" ()
  return $ Prelude.map(\(Only r) -> r) results
                                                        

mkTable :: IO Int64
mkTable = do
  conn <- dbConnect
  execute conn "CREATE TABLE happstack.runs (millimiles INT, duration_sec INT, comment VARCHAR(255))" ()
