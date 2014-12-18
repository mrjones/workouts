{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (liftM, msum)
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
-- sudo apt-get install libmysqlclient-dev
-- cabal install mysql-simple
import Database.MySQL.Simple
import Database.MySQL.Simple.QueryResults (QueryResults, convertResults)
import Database.MySQL.Simple.Result (convert)
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
           , dropTablePage
           , insertFakeDataPage
           , dumpDataPage
           , helloPage
           ]

mkTablePage :: ServerPartT IO Response
mkTablePage = dir "admin" $ dir "mktable" $ do
  i <- liftIO mkTable
  ok (toResponse (executeSqlHtml "create table"i))

dropTablePage :: ServerPartT IO Response
dropTablePage = dir "admin" $ dir "droptable" $ do
  i <- liftIO dropTable
  ok (toResponse (executeSqlHtml "drop table" i))

insertFakeDataPage :: ServerPartT IO Response
insertFakeDataPage = dir "fakedata" $ do
  conn <- liftIO dbConnect
  n <- liftIO (execins conn)
  ok (toResponse (fakeDataHtml n))

dumpDataPage :: ServerPartT IO Response
dumpDataPage = dir "dump" $ do
  conn <- liftIO dbConnect
  runs <- liftIO $ query conn "SELECT comment FROM happstack.runs" ()
  ok (toResponse (simpleMessageHtml (resultsToString runs)))

---------

data Run = Run { comment :: String }

instance QueryResults Run where
  convertResults [fa] [va] = Run (convert fa va)

resultsToString :: [ Run ] -> String
resultsToString = foldr (\r a -> a ++ (comment r)) ""

---------
  
fakeDataHtml :: Int64 -> H.Html
fakeDataHtml n = simpleMessageHtml (show n)

execins :: Connection -> IO Int64
execins conn = execute conn "INSERT INTO happstack.runs (date, miles, duration_sec, incline, comment) VALUES ('2014-12-9', 3.0, 1200, 1.0, 'First post!')" ()

helloPage :: ServerPartT IO Response
helloPage = do
    message <- liftIoMyHead
    helloPage2 message

helloPage2 :: String -> ServerPartT IO Response
helloPage2 msg = ok (toResponse (simpleMessageHtml msg))


liftIoMyHead :: ServerPartT IO String
liftIoMyHead = liftIO $ myHead getMessage

myHead :: IO [String] -> IO String
myHead xs = liftM Prelude.head xs

dbConnect :: IO Connection
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

--
-- Database Admin
--

dropTable :: IO Int64
dropTable = do
  conn <- dbConnect
  execute conn "DROP TABLE happstack.runs" ()

mkTable :: IO Int64
mkTable = do
  conn <- dbConnect
  execute conn "CREATE TABLE happstack.runs (\
               \ id INT NOT NULL AUTO_INCREMENT,\
               \ date DATE,\
               \ miles DECIMAL(5,2),\
               \ duration_sec INT,\
               \ incline DECIMAL(2,1),\
               \ comment VARCHAR(255),\
               \ PRIMARY KEY (id))" ()
--
-- HTML Templates
--
  
executeSqlHtml :: String -> Int64 -> H.Html
executeSqlHtml opname _ =
  H.html $ do
    H.head $ do
      H.title $ "executed database op"
    H.body $ H.toHtml opname

simpleMessageHtml :: String -> H.Html
simpleMessageHtml msg =
  H.html $ do
    H.head $ do
      H.title $ "Hello, world!"
    H.body $ do
      H.toHtml msg

