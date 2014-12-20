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
import Happstack.Server (dir, nullConf, simpleHTTP, toResponse, ok, ServerPart, Response, ServerPartT, look, body, decodeBody, defaultBodyPolicy)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Printf (printf)


main:: IO()
main = do
  simpleHTTP nullConf $ allPages

allPages :: ServerPartT IO Response
allPages = do
  decodeBody (defaultBodyPolicy "/tmp" 0 10240 10240)
  msum [ mkTablePage
       , dropTablePage
       , insertFakeDataPage
       , dumpDataPage
       , newRunFormPage
       , handleNewRunPage
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

newRunFormPage :: ServerPartT IO Response
newRunFormPage = dir "newrun" $ do
  ok $ toResponse $ newRunFormHtml

handleNewRunPage :: ServerPartT IO Response
handleNewRunPage = dir "handlenewrun" $ do
  distance <- body $ look "distance"
  time <- body $ look "time"
  ok $ toResponse $ simpleMessageHtml (printf "%s in %s" distance time)

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

newRunFormHtml :: H.Html
newRunFormHtml =
  H.html $ do
    H.head $ do
      H.title "New Run"
    H.body $ do
      H.form ! A.method "post" ! A.action "/handlenewrun" $ do
        H.label ! A.for "distance" $ H.toHtml ("distance" ::String)
        H.input ! A.type_ "text" ! A.id "distance" ! A.name "distance"
        H.label ! A.for "time" $ H.toHtml ("time" ::String)
        H.input ! A.type_ "text" ! A.id "time" ! A.name "time"
        H.input ! A.type_ "submit"


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
    H.td "Pace"
    H.td "MpH"
    H.td "Note"

dataTableRow :: Run -> H.Html
dataTableRow r = H.tr $ do
  H.td $ H.toHtml $ show $ date r
  H.td $ H.toHtml $ show $ distance r
  H.td $ H.toHtml $ printDuration $ duration r
  H.td $ H.toHtml $ show $ incline r
  H.td $ H.toHtml $ printDuration $ pace r
  H.td $ H.toHtml $ show $ mph r
  H.td $ H.toHtml $ comment r

pace :: Run -> Int
pace r = round $ (fromIntegral (duration r)) / (distance r)

mph :: Run -> Float
mph r = 60 * 60 * (distance r) / (fromIntegral (duration r))

printDuration :: Int -> String
printDuration secs = printf "%d:%02d" (div secs 60) (mod secs 60)

fakeDataHtml :: Int64 -> H.Html
fakeDataHtml n = simpleMessageHtml (show n)

execins :: Connection -> IO Int64
execins conn = execute conn "INSERT INTO happstack.runs (date, miles, duration_sec, incline, comment) VALUES ('2014-12-9', 3.0, 1200, 1.0, 'First post!')" ()

--helloPage :: ServerPartT IO Response
--helloPage = do
--    message <- liftIoMyHead
--    helloPage2 message

--helloPage2 :: String -> ServerPartT IO Response
--helloPage2 msg = ok (toResponse (simpleMessageHtml msg))


liftIoMyHead :: ServerPartT IO String
liftIoMyHead = liftIO $ myHead getMessage

myHead :: IO [String] -> IO String
myHead xs = liftM head xs

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
  return $ map(\(Only r) -> r) results

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

