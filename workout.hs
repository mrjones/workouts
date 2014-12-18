{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (liftM, msum, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
import Data.Time.Calendar (Day)
-- sudo apt-get install libmysqlclient-dev
-- cabal install mysql-simple
import Database.MySQL.Simple
import Database.MySQL.Simple.QueryResults (QueryResults, convertResults)
import Database.MySQL.Simple.Result (convert)
-- cabal install happstack
import Happstack.Server (dir, nullConf, simpleHTTP, toResponse, ok, ServerPart, Response, ServerPartT)
import Text.Blaze.Html5 as H


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
  runs <- liftIO $ query conn "SELECT miles, duration_sec, date, incline, comment FROM happstack.runs" ()
  ok (toResponse (dataTableHtml runs))

---------

data Run = Run { distance :: Float
               , duration :: Int
               , date :: Day
               , incline :: Float
               , comment :: String
               } deriving (Show)

instance QueryResults Run where
  convertResults [f_dist,f_dur,f_date,f_incl,f_comm] [v_dist,v_dur,v_date,v_incl,v_comm] =
    Run (convert f_dist v_dist) (convert f_dur v_dur) (convert f_date v_date) (convert f_incl v_incl) (convert f_comm v_comm)

---------
  
dataTableHtml :: [ Run ] -> H.Html
dataTableHtml rs =
  H.html $ do
    H.head $ do
      H.title $ "Data"
    H.body $ do
      H.table $ do
        dataTableHeader
        mapM_ dataTableRow rs

dataTableHeader :: H.Html
dataTableHeader =
  H.thead $ H.tr $ do
    H.td "Date"
    H.td "Dist"
    H.td "Time"
    H.td "Incline"
    H.td "Note"

dataTableRow :: Run -> H.Html
dataTableRow r = H.tr $ do
  H.td $ toHtml $ show $ date r
  H.td $ toHtml $ show $ distance r
  H.td $ toHtml $ show $ duration r
  H.td $ toHtml $ show $ incline r
  H.td $ toHtml $ comment r

--  foldr (\r a -> a ++ (show r)) ""

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

