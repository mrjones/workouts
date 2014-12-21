{-# LANGUAGE OverloadedStrings #-}

-- sudo apt-get install libmysqlclient-dev
-- cabal install mysql-simple
-- cabal install happstack

import Control.Monad (msum)
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
import Data.Monoid (mconcat)
import Data.Text (splitOn, pack, unpack)
import Data.Time.Calendar (Day, fromGregorianValid, fromGregorian)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, FormatTime)
import Data.Time.LocalTime (LocalTime, utcToLocalTime, getCurrentTimeZone, localDay)
import Database.MySQL.Simple
import Database.MySQL.Simple.QueryResults (QueryResults, convertResults)
import Database.MySQL.Simple.Result (convert)
import Happstack.Server (dir, nullConf, simpleHTTP, toResponse, ok, Response, ServerPartT, look, body, decodeBody, defaultBodyPolicy, queryString)
import System.Locale (defaultTimeLocale)
import Text.Blaze (toValue)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Printf (printf)
import Text.Read (readMaybe)


main:: IO()
main = do
  simpleHTTP nullConf $ allPages

allPages :: ServerPartT IO Response
allPages = do
  decodeBody (defaultBodyPolicy "/tmp" 0 10240 10240)
  msum [ mkTablePage
       , dropTablePage
       , dumpDataPage
       , editRunFormPage
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

dumpDataPage :: ServerPartT IO Response
dumpDataPage = dir "dump" $ do
  conn <- liftIO dbConnect
  runs <- liftIO $ query conn "SELECT miles, duration_sec, date, incline, comment, id FROM happstack.runs" ()
  ok (toResponse (dataTableHtml runs))

newRunFormPage :: ServerPartT IO Response
newRunFormPage = dir "newrun" $ do
  tz <- liftIO $ getCurrentTimeZone
  utcNow <- liftIO $ getCurrentTime
  today <- return $ localDay $ utcToLocalTime tz utcNow
  ok $ toResponse $ runDataHtml Nothing today

editRunFormPage :: ServerPartT IO Response
editRunFormPage = dir "editrun" $ do
  id <- queryString $ look "id"
  conn <- liftIO dbConnect
  runs <- liftIO $ (query conn "SELECT miles, duration_sec, date, incline, comment, id FROM happstack.runs WHERE id = (?)" [id])
  ok $ toResponse $ runDataHtml (Just (head runs)) (fromGregorian 2014 1 1)

handleNewRunPage :: ServerPartT IO Response
handleNewRunPage = dir "handlenewrun" $ do
  distanceS <- body $ look "distance"
  timeS <- body $ look "time"
  dateS <- body $ look "date"
  inclineS <- body $ look "incline"
  commentS <- body $ look "comment"
  run <- return $ (parseRun distanceS timeS dateS inclineS commentS)
  conn <- liftIO dbConnect
  _ <- liftIO $ storeRun conn run
  ok $ toResponse $ simpleMessageHtml (show run)

parseRun :: String -> String -> String -> String -> String -> Maybe Run
parseRun distanceS durationS dateS inclineS commentS = do
  distance <- readMaybe distanceS :: Maybe Float
  incline <- readMaybe inclineS :: Maybe Float
  duration <- parseDuration durationS
  date <- parseDate dateS
  Just (Run distance duration date incline commentS 0)

parseDuration :: String -> Maybe Int
parseDuration input = do
  parts <- Just (splitOn (pack ":") (pack input))
  case parts of
    [minS,secS] -> do
      min <- readMaybe (unpack minS) :: Maybe Int
      sec <- readMaybe (unpack secS) :: Maybe Int
      Just (min * 60 + sec)
    _ -> Nothing

parseDate :: String -> Maybe Day
parseDate input = do
  parts <- Just (splitOn (pack "-") (pack input))
  case parts of
    [yearS,monthS,dayS] -> do
      year <- readMaybe (unpack yearS) :: Maybe Integer
      month <- readMaybe (unpack monthS) :: Maybe Int
      day <- readMaybe (unpack dayS) :: Maybe Int
      fromGregorianValid year month day
    _ -> Nothing

storeRun :: Connection -> Maybe Run -> IO Int64
storeRun conn mr = case mr of
  Just r -> execute conn "INSERT INTO happstack.runs (date, miles, duration_sec, incline, comment) VALUES (?, ?, ?, ?, ?)"
            (date r, distance r, duration r, incline r, comment r)
  Nothing -> return 0


---------

data Run = Run { distance :: Float
               , duration :: Int
               , date :: Day
               , incline :: Float
               , comment :: String
               , runid :: Int
               } deriving (Show)

instance QueryResults Run where
  convertResults [f_dist,f_dur,f_date,f_incl,f_comm,f_id] [v_dist,v_dur,v_date,v_incl,v_comm,v_id] =
    Run (convert f_dist v_dist) (convert f_dur v_dur) (convert f_date v_date) (convert f_incl v_incl) (convert f_comm v_comm) (convert f_id v_id)

---------

runDataHtml :: Maybe Run -> Day -> H.Html
runDataHtml run today =
  H.html $ do
    H.head $ do
      H.title "New Run"
    H.body $ do
      H.form ! A.method "post" ! A.action "/handlenewrun" $ do
        H.table $ do
          mconcat $ map (runDataFormRow run)
                   [ ("Distance", "distance", "text", "", (show . distance), [])
                   , ("Time", "time", "text", "", (printDuration . duration), [])
                   , ("Incline", "incline", "text", "", (show . incline), [])
                   , ("Date", "date", "date", formatTimeForInput today, (formatTimeForInput . date), [])
                   , ("Comment", "comment", "text", "", comment, [
                         (A.size (toValue (75 :: Int)))])
                   ]
        H.input ! A.type_ "submit"

runDataFormRow :: Maybe Run -> (String, String, String, String, (Run -> String), [H.Attribute]) -> H.Html
runDataFormRow mrun (name, id, formType, defaultValue, extractValue, extraAs) =
  let defaultAs = 
        [ A.type_ (toValue formType)
        , A.id (toValue id)
        , A.name (toValue id)
        , case mrun of
             Just run -> A.value $ toValue $ extractValue run
             Nothing -> A.value $ toValue defaultValue
        ] in
  H.tr $ do
    H.td $
      H.label ! A.for (toValue id) $ H.toHtml name
    H.td $
      foldr (flip (!)) H.input (defaultAs ++ extraAs)

formatTimeForInput :: FormatTime t => t -> String
formatTimeForInput time = formatTime defaultTimeLocale "%Y-%m-%d" time

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
    mconcat $ map (H.td . H.b)
      [ "Date", "Dist", "Time", "Incline", "Pace", "MpH", "Node", "Edit" ]

dataTableRow :: Run -> H.Html
dataTableRow r = H.tr $ do
  H.td $ H.toHtml $ show $ date r
  H.td $ H.toHtml $ show $ distance r
  H.td $ H.toHtml $ printDuration $ duration r
  H.td $ H.toHtml $ show $ incline r
  H.td $ H.toHtml $ printDuration $ pace r
  H.td $ H.toHtml $ show $ mph r
  H.td $ H.toHtml $ comment r
  H.td $ do
    "["
    H.a ! A.href (toValue ("/editrun?id=" ++ (show (runid r)))) $ "Edit"
    "]"

pace :: Run -> Int
pace r = round $ (fromIntegral (duration r)) / (distance r)

mph :: Run -> Float
mph r = 60 * 60 * (distance r) / (fromIntegral (duration r))

printDuration :: Int -> String
printDuration secs = printf "%d:%02d" (div secs 60) (mod secs 60)

dbConnect :: IO Connection
dbConnect = connect defaultConnectInfo
    { connectUser = "happstack"
    , connectPassword = "happstack"
    , connectDatabase = "happstack"
    }

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

