{-# LANGUAGE OverloadedStrings #-}

module Workouts(WorkoutConf(..), workoutMain, computeRest,rankAsc,parseDuration,parseLookback) where

import Control.Applicative (optional)
import Control.Monad (msum)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Lazy (State, state, runState)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import qualified Data.Aeson as J ((.:))
import qualified Data.Aeson as JSON (eitherDecode, fromJSON, FromJSON(..), Result(..), Object, Value(..))
import Data.Aeson.Types (emptyObject, typeMismatch)
import qualified Data.ByteString.Char8 as C8 (pack)
import qualified Data.ByteString.Lazy.Char8 as C8L (fromStrict)
import qualified Data.ByteString.Base64 as BS64 (decodeLenient)
--import qualified Data.Csv as CSV (decodeByName, FromNamedRecord(..), (.:), Parser(..), NamedRecord(..))
import Data.Int (Int64)
import qualified Data.HashMap.Lazy (lookupDefault)
import Data.List (sort, findIndex, zip5, intersperse, sortBy)
import Data.Maybe (fromJust, fromMaybe)
import Data.String (fromString)
import qualified Data.Text as Text (splitOn, pack, unpack, Text)
import qualified Data.Text.Encoding as TextEnc (encodeUtf8)
import Data.Time.Calendar (Day, fromGregorianValid, fromGregorian, diffDays)
import Data.Time.Clock (diffUTCTime, getCurrentTime, UTCTime(..), NominalDiffTime)
import Data.Time.Format (defaultTimeLocale, formatTime, FormatTime)
import Data.Time.LocalTime (utcToLocalTime, getCurrentTimeZone, localDay)
import Database.MySQL.Simple
import Database.MySQL.Simple.QueryResults (QueryResults, convertError, convertResults)
import Database.MySQL.Simple.Result (convert)
import Happstack.Server (dir, nullConf, simpleHTTPWithSocket, toResponse, ok, Response, ServerPartT, look, body, decodeBody, defaultBodyPolicy, queryString, seeOther, mkCookie, addCookie, readCookieValue, CookieLife(Session), expireCookie, withHost, port, bindPort, checkRqM, serveFile, asContentType)
import Happstack.Server.Internal.Types (rqUri)
import Happstack.Server.Monads (askRq)
import qualified Network.HTTP.Client()
import Network.HTTP.Simple (getResponseBody, httpJSON, parseRequest_, setRequestBodyURLEncoded, setRequestMethod)
import Text.Blaze (toValue)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Printf (printf)
import Text.Read (readMaybe)

data WorkoutConf = WorkoutConf { wcGoogleClientId :: String
                               , wcGoogleClientSecret :: String
                               , wcAdminKind :: String
                               , wcAdminId :: String
                               , wcPort :: Int
                               , wcMysqlHost :: String
                               , wcMysqlUser :: String
                               , wcMysqlPassword :: String
                               , wcStaticDir :: String
                               } deriving (Show)

workoutMain :: WorkoutConf -> IO ()
workoutMain wc = do
  let httpConf = nullConf {  port = wcPort wc }
  socket <- bindPort nullConf { port = wcPort wc }
  putStrLn $ "Serving on port: " ++ (show (wcPort wc))
  simpleHTTPWithSocket socket httpConf $ allPages wc


--
-- Data types
--

data RunMeta = RunMeta { metaDaysOff :: Integer
                       , metaScoreRank :: Int
                       , metaPaceRank :: Int
                       , metaMiles7 :: Float
                       , metaMiles56 :: Float
                       }

data Run = Run { runDistance :: Float
               , runDuration :: Int
               , runDate :: Day
               , runIncline :: Float
               , runComment :: String
               , runId :: Int
               , runUserId :: Int
               } deriving (Show)

data User = User { userId :: Int,
                   userName :: String,
                   userKind :: String,
                   foreignUserId :: String } deriving (Read, Show)

data MutationKind = Create | Modify | Delete deriving (Read, Show)

data IdentityProvider = Google deriving (Show)

data Identity = Identity{ displayName :: String
                        , uniqueId :: String
                        , _unusedIdProvider :: IdentityProvider
                        } deriving (Show)

data MonthlyData = MonthlyData { monthlyYear :: Int
                               , monthlyMonth :: Int
                               , monthlyMiles :: Float
                               }

--
-- Routing / handlers
--

mb :: Int64
mb = 1024 * 1024

staticPath :: WorkoutConf -> String -> String
staticPath c p = (wcStaticDir c) ++ "/" ++ p

allPages :: WorkoutConf -> ServerPartT IO Response
allPages wc =
  withHost (\host -> do
               requestStart <- liftIO $ getCurrentTime
               decodeBody (defaultBodyPolicy "/tmp" (10 * mb) (10 * mb) (10 * mb))
               req <- askRq
               liftIO $ putStrLn (rqUri req)
               redirectUrl <- return $ "http://" ++ host ++ "/handlelogin"
               msum [ dir "logout" $ logoutPage
                    , dir "js" $ serveFile (asContentType "text/javascript") (staticPath wc "js/workouts.js")
                    , dir "css" $ serveFile (asContentType "text/css") (staticPath wc "css/workouts.css")
                    , dir "favicon.ico" $ serveFile (asContentType "image/x-icon") (staticPath wc "favicon.ico")
                    , databasePages wc redirectUrl requestStart
                    , do loginUrl <- return $ googleLoginUrl (wcGoogleClientId wc) redirectUrl ""
                         ok $ toResponse $ notLoggedInHtml loginUrl
                    ])

databasePages :: WorkoutConf -> String -> UTCTime -> ServerPartT IO Response
databasePages wc redirectUrl requestStart = do
  conn <- liftIO $ dbConnect (wcMysqlHost wc) (wcMysqlUser wc) (wcMysqlPassword wc)
  msum [ dir "admin" $ dir "mkdb" $ mkDbPage conn
       , loggedInPages conn (wcAdminKind wc) (wcAdminId wc) requestStart
       , dir "handlelogin" $ handleLoginPage conn (wcGoogleClientId wc) (wcGoogleClientSecret wc) redirectUrl
       ]

loggedInPages :: Connection -> String -> String -> UTCTime -> ServerPartT IO Response
loggedInPages conn adminKind adminId requestStart = do
  user <- (readCookieValue "userid") `checkRqM` (userWithId conn)
  msum [ dir "admin" $ dir "refreshdb" $ requireAdmin conn adminKind adminId (refreshDbPage conn)
       , dir "rundata" $ runDataPage conn user user requestStart
       , dir "editrun" $ editRunFormPage conn user
       , dir "newrun" $ newRunFormPage user
       , dir "handlemutaterun" $ handleMutateRunPage conn user
       , dir "chart" $ dir "mpw" $ mpwChartPage conn user
       , dir "import" $ importFormPage
--       , dir "handleimport" $ handleImportPage conn user
       , dir "peek" $ peekPage conn user requestStart
       , dir "monthly" $ monthlyPage conn user
       , runDataPage conn user user requestStart
       ]

importFormPage :: ServerPartT IO Response
importFormPage = ok $ toResponse $ importFormHtml

--data CsvRunRecord = CsvRunRecord { csvRunDate :: String
--                                 , csvRunDist :: Float
--                                 , csvRunDuration :: String
--                                 , csvRunIncline :: Maybe Float
--                                 , csvRunComment :: Maybe String } deriving (Show)

--instance CSV.FromNamedRecord CsvRunRecord where
--  parseNamedRecord record =
--    CsvRunRecord <$>
--    record CSV..: "Date" <*>
--    record CSV..: "Distance" <*>
--    record CSV..: "Time" <*>
--    record CSV..: "Inc" <*>
--    record CSV..: "Comment"
--
--parseCsvRun :: User -> CsvRunRecord -> Maybe Run
--parseCsvRun owner csv = do
--  date <- parseDateDDMMYYYYslash (csvRunDate csv)
--  duration <- parseDuration (csvRunDuration csv)
--  inc <- return $ fromMaybe 0.0 (csvRunIncline csv)
--  comment <- return $ fromMaybe "" (csvRunComment csv)
--  return $ Run (csvRunDist csv) duration date inc comment 0 (userId owner)
--
--handleImportPage :: Connection -> User -> ServerPartT IO Response
--handleImportPage conn user = do
--  (fname, _, _) <- lookFile "filedata"
--  csvData <- liftIO $ BL.readFile fname
--  case CSV.decodeByName csvData of
--    Left err -> ok $ toResponse $ simpleMessageHtml ("ERROR: " ++ err)
--    Right (_, v) -> do
--      liftIO $ Vector.forM v (\r -> do
--                                 run <- return $ (parseCsvRun user r)
--                                 storeRun conn run (Just Create))
--      ok $ toResponse $ simpleMessageHtml "foo"

parseLookback :: Maybe String -> Integer
parseLookback mStr = fromMaybe 36500 (mStr >>= readMaybe)

mpwChartPage :: Connection -> User -> ServerPartT IO Response
mpwChartPage conn user = do
  lookbackP <- optional $ look "lookback_days"
  lookback <- return $ parseLookback lookbackP
  runs <- liftIO $ query conn "SELECT miles, duration_sec, date, incline, comment, id, user_id FROM happstack.runs WHERE user_id = (?) AND date > (NOW() - INTERVAL (?) DAY) ORDER BY date ASC" ((userId user), lookback)
  annotated <- return $ annotate runs
  ok $ toResponse $ mpwChartHtml annotated lookback user


logoutPage :: ServerPartT IO Response
logoutPage = do
  expireCookie "userid"
  seeOther ("/" :: String) $ toResponse ("Logging out..." :: String)

handleLoginPage :: Connection -> String -> String -> String -> ServerPartT IO Response
handleLoginPage conn clientid secret redirectUrl = do
  code <- look "code"
  mid <- liftIO $ getGoogleId clientid secret code redirectUrl
  case mid of
    Left err -> ok $ toResponse $ simpleMessageHtml ("Login failed: " ++ err)
    Right userid -> do
      mu <- liftIO $ findOrInsertGoogleUser conn (displayName userid) (uniqueId userid)
      case mu of
        Nothing -> ok $ toResponse $ simpleMessageHtml "user <-> db failed"
        -- TODO(mrjones): this is insanely insecure
        Just u -> do addCookie Session (mkCookie "userid" (show $ userId u))
                     seeOther ("/" :: String) $ toResponse ("Logging in..." :: String)

requireAdmin :: Connection -> String -> String -> ServerPartT IO Response -> ServerPartT IO Response
requireAdmin conn adminKind adminId protectedPage = do
  uid <- readCookieValue "userid"
  liftIO $ putStrLn (show uid)
  mu <- liftIO $ findUserById conn uid
  liftIO $ putStrLn (show mu)
  case mu of
    Nothing -> ok $ toResponse $ simpleMessageHtml "NOT LOGGED IN"
    Just u -> if ((adminKind == (userKind u)) &&
                  (adminId == (foreignUserId u)))
              then protectedPage
              else ok $ toResponse $ simpleMessageHtml "NOT ADMIN"

refreshDbPage :: Connection -> ServerPartT IO Response
refreshDbPage conn = do
  dropN <- liftIO $ dropTable conn
  runsN <- liftIO $ mkRunTable conn
  usersN <- liftIO $ mkUserTable conn
  ok (toResponse (executeSqlHtml "create table" (dropN + runsN + usersN)))

mkDbPage :: Connection -> ServerPartT IO Response
mkDbPage conn = do
  runs <- liftIO $ mkRunTable conn
  users <- liftIO $ mkUserTable conn
  ok (toResponse (executeSqlHtml "create table" (runs + users)))

sortKey :: Maybe String -> String
sortKey mKey = case mKey of
  Just "distance" -> "distance"
  Just "time" -> "time"
  Just "incline" -> "incline"
  Just "pace" -> "pace"
  Just "mph" -> "mph"
  Just "rest" -> "rest"
  Just "score" -> "score"
  Just "score_rank" -> "score_rank"
  Just "pace_rank" -> "pace_rank"
  Just "miles7" -> "miles7"
  _ -> "date"

sortReverse :: Maybe String -> Bool
sortReverse mDir = if mDir == Just "True" then True else False

genericSorter :: String -> Bool -> (Run, RunMeta) -> (Run, RunMeta) -> Ordering
genericSorter key reverseDir (r1, m1) (r2, m2) =
  let
    (rA, mA) = if not reverseDir then (r1, m1) else (r2, m2)
    (rB, mB) = if not reverseDir then (r2, m2) else (r1, m1)
  in case key of
    "distance" -> compare (runDistance rA) (runDistance rB)       -- descending
    "time" -> compare (runDuration rA) (runDuration rB)           -- descending
    "incline" -> compare (runIncline rA) (runIncline rB)          -- descending
    "pace" -> compare (pace rB) (pace rA)                         -- ascending
    "mph" -> compare (mph rA) (mph rB)                            -- descending
    "rest" -> compare (metaDaysOff mA) (metaDaysOff mB)           -- descending
    "score" -> compare (scoreRun rA) (scoreRun rB)                -- descending
    "score_rank" -> compare (metaScoreRank mB) (metaScoreRank mA) -- ascending
    "pace_rank" -> compare (metaPaceRank mB) (metaPaceRank mA)    -- ascending
    "miles7" -> compare (metaMiles7 mA) (metaMiles7 mB)           -- descending
    _ -> compare (runDate rA) (runDate rB)                        -- descending

maybeFindUserWithId :: Connection -> Maybe String -> IO (Maybe User)
maybeFindUserWithId conn mId = runMaybeT $ do
  userid <- MaybeT $ return mId
  MaybeT $ findUserById conn userid

monthlyPage :: Connection -> User -> ServerPartT IO Response
monthlyPage conn user = do
  mSortBy <- optional $ look "sort_by"
  orderBySql <- return $ if mSortBy == Just "miles" then ("miles DESC" :: String) else ("year DESC, month DESC" :: String)
  monthlyData <- liftIO $ query conn ((fromString ("SELECT YEAR(date) AS year, MONTH(date) AS month, SUM(miles) as miles FROM happstack.runs WHERE user_id = (?) GROUP BY year, month ORDER BY " ++ orderBySql)) :: Query) [(userId user)]
  ok $ toResponse $ monthlyDataHtml monthlyData user

peekUser :: Connection -> User -> Maybe String -> IO User
peekUser conn fallback mId =
  fmap (fromMaybe fallback) (maybeFindUserWithId conn mId)

peekPage :: Connection -> User -> UTCTime -> ServerPartT IO Response
peekPage conn loggedInUser requestStart = do
  mPeekId <- optional $ look "peek_id"
  displayUser <- liftIO $ peekUser conn loggedInUser mPeekId
  runDataPage conn loggedInUser displayUser requestStart

runDataPage :: Connection -> User -> User -> UTCTime -> ServerPartT IO Response
runDataPage conn loggedInUser displayUser requestStart = do
  mSortBy <- optional $ look "sort_by"
  mSortReverse <- optional $ look "sort_reverse"
  sorter <- return $ genericSorter (sortKey mSortBy) (sortReverse mSortReverse)
  runs <- liftIO $ query conn "SELECT miles, duration_sec, date, incline, comment, id, user_id FROM happstack.runs WHERE user_id = (?) ORDER BY date ASC" [(userId displayUser)]
  queryDone <- liftIO $ getCurrentTime
  ok (toResponse (dataTableHtml loggedInUser displayUser (sortBy sorter (annotate runs)) (diffUTCTime queryDone requestStart) (sortKey mSortBy) (sortReverse mSortReverse)))

newRunFormPage :: User -> ServerPartT IO Response
newRunFormPage user = do
  tz <- liftIO $ getCurrentTimeZone
  utcNow <- liftIO $ getCurrentTime
  today <- return $ localDay $ utcToLocalTime tz utcNow
  ok $ toResponse $ runDataHtml user Nothing today

editRunFormPage :: Connection -> User -> ServerPartT IO Response
editRunFormPage conn user = do
  userid <- queryString $ look "id"
  runs <- liftIO $ (query conn "SELECT miles, duration_sec, date, incline, comment, id, user_id FROM happstack.runs WHERE id = (?)" [userid])
  ok $ toResponse $ runDataHtml user (Just (head runs)) (fromGregorian 2014 1 1)

handleMutateRunPage :: Connection -> User -> ServerPartT IO Response
handleMutateRunPage conn user = do
  mutationKindS <- body $ look "button"
  distanceS <- body $ look "distance"
  timeS <- body $ look "time"
  dateS <- body $ look "date"
  inclineS <- body $ look "incline"
  commentS <- body $ look "comment"
  mutationKind <- return $ readMaybe mutationKindS
  idS <- body $ look "id"
  run <- return $ (parseRun distanceS timeS dateS inclineS commentS idS (userId user))
  n <- liftIO $ storeRun conn run mutationKind
  case n of
    1 -> seeOther ("/rundata" :: String) (toResponse ("Redirecting to run list" :: String))
    0 -> ok $ toResponse $ simpleMessageHtml "error"
    rows -> ok $ toResponse $ simpleMessageHtml ("Expected to update 1 row, but got: " ++ (show rows))
--
-- Google login flow
--

userWithId :: Connection -> String -> ServerPartT IO (Either String User)
userWithId conn userid = do
  maybeuser <- liftIO $ findUserById conn userid
  return $ case maybeuser of
    Nothing -> Left ("No user with id: " ++ userid)
    Just user -> Right user

googleLoginUrl :: String -> String -> String -> String
googleLoginUrl clientid redirect stateData =
  printf "https://accounts.google.com/o/oauth2/auth?client_id=%s&response_type=code&scope=openid%%20email&redirect_uri=%s&state=%s" clientid redirect stateData

getGoogleIdUrl :: String
getGoogleIdUrl = "https://www.googleapis.com/oauth2/v3/token"

data JWTPayload = JWTPayload { jwtSub :: String
                             , jwtEmail :: String
                             } deriving (Show)

instance JSON.FromJSON JWTPayload where
  parseJSON (JSON.Object o) = JWTPayload <$>
                         o J..: "sub" <*>
                         o J..: "email"
  parseJSON v = typeMismatch "jwt" v

decodeText :: JSON.FromJSON a => Text.Text -> Either String a
decodeText inTxt =
  JSON.eitherDecode (C8L.fromStrict (BS64.decodeLenient (TextEnc.encodeUtf8 inTxt)))

extractTokenFromResponse :: JSON.Value -> Either String String
extractTokenFromResponse responseJson =
  let tokenResult = do
        obj <- (JSON.fromJSON responseJson) :: JSON.Result JSON.Object
        tokenValue <- return $ Data.HashMap.Lazy.lookupDefault emptyObject "id_token" obj
        JSON.fromJSON tokenValue :: JSON.Result String
  in case tokenResult of
       JSON.Success t -> Right t
       JSON.Error err -> Left err

getGoogleJWTToken  :: String -> String -> String -> String -> IO (Either String String)
getGoogleJWTToken clientid secret code redirectUrl =
  do
    let request =
          setRequestMethod "POST"
          $ setRequestBodyURLEncoded [("code", C8.pack code),
                                      ("client_id", C8.pack clientid),
                                      ("client_secret", C8.pack secret),
                                      ("redirect_uri", C8.pack redirectUrl),
                                      ("grant_type", "authorization_code")
                                     ]
          $ parseRequest_ getGoogleIdUrl
    response <- httpJSON request
    return $ extractTokenFromResponse (getResponseBody response :: JSON.Value)

getGoogleId :: String -> String -> String -> String -> IO (Either String Identity)
getGoogleId clientid secret code redirectUrl = do
  maybeIdToken <- getGoogleJWTToken clientid secret code redirectUrl
  return $ do
    idToken <- (maybeIdToken :: Either String String)
    encodedToken <- return $ Text.pack idToken
    encodedParts <- return $ Text.splitOn "." encodedToken
    -- TODO(mrjones): verify the signature with the algorithm named in
    -- the header
    payload <- (decodeText (head (tail encodedParts)) :: Either String JWTPayload)
    return (Identity (jwtEmail payload) (jwtSub payload) Google)

findOrInsertGoogleUser :: Connection -> String -> String -> IO (Maybe User)
findOrInsertGoogleUser conn email sub = do
  fromDb <- findGoogleUser conn sub
  case fromDb of
    Nothing -> do _ <- insertGoogleUser conn email sub
                  findGoogleUser conn sub
    Just u -> return $ Just u


findUserById :: Connection -> String -> IO (Maybe User)
findUserById conn userid = do
  users <- query conn "SELECT id, display, kind, foreign_id FROM happstack.users WHERE id = (?)" [userid]
  case users of
    [] -> return $ Nothing
    (u:_) -> return $ Just u

findGoogleUser :: Connection -> String -> IO (Maybe User)
findGoogleUser conn userid = do
  users <- query conn "SELECT id, display, kind, foreign_id FROM happstack.users WHERE kind = 'google' AND foreign_id = (?)" [userid]
  case users of
    [] -> return $ Nothing
    (u:_) -> return $ Just u


insertGoogleUser :: Connection -> String -> String -> IO Int64
insertGoogleUser conn userdisplay userid =
    execute conn "INSERT INTO happstack.users (display, kind, foreign_id) VALUES (?, 'google', ?)" (userdisplay, userid)

--
-- Misc application logic
--

annotate :: [Run] -> [(Run, RunMeta)]
annotate rs = zip rs (annotate2 rs)

annotate2 :: [Run] -> [RunMeta]
annotate2 rs = map buildMeta
               (zip5
                (computeRest (map runDate rs))
                (rankDesc (map scoreRun rs))
                (rankAsc (map pace rs))
                (trailingMileage 7 7 rs)
                (trailingMileage 56 7 rs))



buildMeta :: (Integer, Int, Int, Float, Float) -> RunMeta
buildMeta (rest, score, paceValue, miles7, miles56) =
  RunMeta rest score paceValue miles7 miles56


trailingOne :: Integer -> Integer -> Run -> State [Run] Float
trailingOne windowSize denominatorDays nextRun =
  let scale = (fromIntegral denominatorDays) / (fromIntegral windowSize)
  in state $ (\rs -> (foldr (\candidate (distAcc, outAcc) ->
                              if (diffDays (runDate nextRun) (runDate candidate) < windowSize)
                              then ((distAcc + (scale * (runDistance candidate))), candidate:outAcc)
                              else (distAcc, outAcc)) (0.0, []) (rs ++ [nextRun])))

trailingAll :: Integer -> Integer -> [Run] -> State [Run] [Float]
trailingAll windowSize denominatorDays runs =
  mapM (trailingOne windowSize denominatorDays) runs

trailingMileage :: Integer -> Integer -> [Run] -> [Float]
trailingMileage windowSize denominatorDays runs =
  fst $ runState (trailingAll windowSize denominatorDays runs) []

restDays :: Day -> Day -> Integer
restDays d1 d2 = max ((diffDays d1 d2) - 1) 0

computeRest :: [Day] -> [Integer]
computeRest ds = zipWith restDays ds ((head ds):ds)

rankAsc :: Ord a => [a] -> [Int]
rankAsc = rank id

rankDesc :: Ord a => [a] -> [Int]
rankDesc = rank reverse

rank :: Ord a => ([a] -> [a]) -> [a] -> [Int]
rank order ins =
  let sorted = order (sort ins)
  in map (+1) $ fromJust . sequence $
     map (\x -> findIndex ((==) x) sorted) ins

-- 1000 * 4 * (distance^1.06)/(time_minutes)
scoreRun :: Run -> Float
scoreRun r =
  let time_minutes = (fromIntegral (runDuration r)) / 60
  in 1000 * 4 * ((runDistance r) ** (1.06)) / time_minutes

parseRun :: String -> String -> String -> String -> String -> String -> Int -> Maybe Run
parseRun distanceS durationS dateS inclineS commentS idS userid = do
  distance <- readMaybe distanceS :: Maybe Float
  incline <- readMaybe inclineS :: Maybe Float
  duration <- parseDuration durationS
  date <- parseDateYYYYMMDDhyph dateS
  runid <- readMaybe idS :: Maybe Int
  Just (Run distance duration date incline commentS runid userid)

parseDuration :: String -> Maybe Int
parseDuration input = do
  parts <- Just (Text.splitOn (Text.pack ":") (Text.pack input))
  case parts of
    [minS,secS] -> parseHourMinSec "0" minS secS
    [hourS,minS,secS] -> parseHourMinSec hourS minS secS
    _ -> Nothing

parseHourMinSec :: Text.Text -> Text.Text -> Text.Text -> Maybe Int
parseHourMinSec hourS minS secS = do
  hour <- readMaybe (Text.unpack hourS) :: Maybe Int
  minute <- readMaybe (Text.unpack minS) :: Maybe Int
  sec <- readMaybe (Text.unpack secS) :: Maybe Int
  Just (hour * 3600 + minute * 60 + sec)

parseDateYYYYMMDDhyph :: String -> Maybe Day
parseDateYYYYMMDDhyph input = do
  parts <- Just (Text.splitOn (Text.pack "-") (Text.pack input))
  case parts of
    [yearS,monthS,dayS] -> do
      year <- readMaybe (Text.unpack yearS) :: Maybe Integer
      month <- readMaybe (Text.unpack monthS) :: Maybe Int
      day <- readMaybe (Text.unpack dayS) :: Maybe Int
      fromGregorianValid year month day
    _ -> Nothing

formatTimeForInput :: FormatTime t => t -> String
formatTimeForInput time = formatTime defaultTimeLocale "%Y-%m-%d" time

pace :: Run -> Float
pace r = (fromIntegral (runDuration r)) / (runDistance r)

mph :: Run -> Float
mph r = 60 * 60 * (runDistance r) / (fromIntegral (runDuration r))

printDuration :: Int -> String
printDuration secs = printf "%d:%02d" (div secs 60) (mod secs 60)

storeRun :: Connection -> Maybe Run -> Maybe MutationKind -> IO Int64
storeRun conn mrun mkind = fromMaybe (return 0) $ do
  run <- mrun
  kind <- mkind
  return $ storeRun2 conn run kind

--
-- Database logic
--

dbConnect :: String -> String -> String -> IO Connection
dbConnect hostname user password = do
  start <- getCurrentTime
  conn <- connect defaultConnectInfo
    { connectUser = user
    , connectPassword = password
    , connectDatabase = "happstack"
    , connectHost = hostname
    }
  end <- getCurrentTime
  putStrLn $ "DBConnect time: " ++ (show (diffUTCTime end start))
  return conn

instance QueryResults Run where
  convertResults [f_dist,f_dur,f_date,f_incl,f_comm,f_id,f_uid] [v_dist,v_dur,v_date,v_incl,v_comm,v_id,v_uid] =
    Run (convert f_dist v_dist) (convert f_dur v_dur) (convert f_date v_date) (convert f_incl v_incl) (convert f_comm v_comm) (convert f_id v_id) (convert f_uid v_uid)
  convertResults fs vs = convertError fs vs 7

instance QueryResults User where
  convertResults [f_id, f_disp, f_kind, f_fid] [v_id, v_disp, v_kind, v_fid] =
    User (convert f_id v_id) (convert f_disp v_disp) (convert f_kind v_kind) (convert f_fid v_fid)
  convertResults fs vs = convertError fs vs 4

instance QueryResults MonthlyData where
  convertResults [f_year, f_month, f_miles] [v_year, v_month, v_miles] =
    MonthlyData (convert f_year v_year) (convert f_month v_month) (convert f_miles v_miles)
  convertResults fs vs = convertError fs vs 3

storeRun2 :: Connection -> Run -> MutationKind -> IO Int64
storeRun2 conn r kind =
    case kind of
      Create -> execute conn
                "INSERT INTO happstack.runs (date, miles, duration_sec, incline, comment, user_id) VALUES (?, ?, ?, ?, ?, ?)"
                (runDate r, runDistance r, runDuration r, runIncline r, runComment r, runUserId r)
      Modify -> execute conn
                "UPDATE happstack.runs SET date=?, miles=?, duration_sec=?, incline=?, comment=?, user_id=? WHERE id=?"
                (runDate r, runDistance r, runDuration r, runIncline r, runComment r, runUserId r, runId r)
      Delete -> execute conn
                "DELETE FROM happstack.runs WHERE id = (?)"
                [runId r]

--
-- Database Admin
--

dropTable :: Connection -> IO Int64
dropTable conn = do
  r <- execute conn "DROP TABLE happstack.runs" ()
  u <- execute conn "DROP TABLE happstack.users;" ()
  return $ r + u

mkUserTable :: Connection -> IO Int64
mkUserTable conn = do
  execute conn "CREATE TABLE happstack.users (\
               \ id INT NOT NULL AUTO_INCREMENT,\
               \ display VARCHAR(255),\
               \ kind VARCHAR(255),\
               \ foreign_id VARCHAR(255),\
               \ PRIMARY KEY (id), \
               \ UNIQUE (kind, foreign_id))" ()

mkRunTable :: Connection -> IO Int64
mkRunTable conn = do
  execute conn "CREATE TABLE happstack.runs (\
               \ id INT NOT NULL AUTO_INCREMENT,\
               \ date DATE,\
               \ miles DECIMAL(5,2),\
               \ duration_sec INT,\
               \ incline DECIMAL(2,1),\
               \ comment VARCHAR(255),\
               \ user_id INT, \
               \ PRIMARY KEY (id))" ()
--
-- HTML Templates
--

headHtml :: String -> H.Html
headHtml title =
  H.head $ do
    H.title $ H.toHtml title
    H.script ! A.type_ "text/javascript" ! A.src "https://www.google.com/jsapi" $ ""
    H.script ! A.type_ "text/javascript" ! A.src "/js/workouts.js" $ ""
    H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/css/workouts.css"
    H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "http://fonts.googleapis.com/css?family=Roboto:400,700"

executeSqlHtml :: String -> Int64 -> H.Html
executeSqlHtml opname _ =
  H.html $ do
    headHtml "Executed database op"
    H.body $ H.toHtml opname

simpleMessageHtml :: String -> H.Html
simpleMessageHtml msg =
  H.html $ do
    headHtml "Hello, world!"
    H.body $ do
      H.toHtml msg

headerBarHtml :: User -> User -> H.Html
headerBarHtml loggedInUser displayUser =
  H.div ! A.class_ "header" $
    -- TODO(mrjones): display special header if loggedIn != display
    let name =
          if (userId loggedInUser) == (userId displayUser)
          then userName loggedInUser
          else (userName loggedInUser) ++ " (" ++ (userName displayUser) ++ ")"
      in do H.div ! A.class_ "username" $ H.toHtml name
            H.a ! A.href "/logout" $ "Logout"
            H.a ! A.href "/newrun" $ "+ New"
            H.a ! A.href "/rundata" $ "All"
            H.a ! A.href "/chart/mpw" $ "Charts"
            H.a ! A.href "/monthly" $ "Monthly"

dataTableHtml :: User -> User -> [(Run, RunMeta)] -> NominalDiffTime -> String -> Bool -> H.Html
dataTableHtml loggedInUser displayUser rs t currentSort currentReverse =
  H.html $ do
    headHtml "Run data"
    H.body $ do
      headerBarHtml loggedInUser displayUser
      H.table ! A.class_ "datatable" $ do
        dataTableHeader currentSort currentReverse
        mapM_ dataTableRow (reverse rs)
      H.div ! A.id "chart_div" $ ""
      H.div ! A.id "debug" $ H.toHtml $ ("Server time: " ++ (show t))


data TableColumn = TableColumn { colHumanName :: String
                               , colSortableKey :: String
                               , colSortable :: Bool
                               , colDataFn :: ((Run, RunMeta) -> H.Html)
                               }
editCellHtml :: Int -> H.Html
editCellHtml runid = do
  "["
  H.a ! A.href (toValue ("/editrun?id=" ++ (show runid))) $ "Edit"
  "]"

cols :: [TableColumn]
cols = [ TableColumn "Date" "date" True (\(r,_) -> H.toHtml $ formatTime defaultTimeLocale "%Y-%b-%d" (runDate r))
       , TableColumn "Day" "" False(\(r,_) -> H.toHtml $ formatTime defaultTimeLocale "%a" (runDate r))
       , TableColumn "Dist" "distance" True (\(r,_) -> H.toHtml $ show $ runDistance r)
       , TableColumn "Time" "time" True (\(r,_) -> H.toHtml $ printDuration $ runDuration r)
       , TableColumn "Incline" "incline" True (\(r,_) -> H.toHtml $ show $ runIncline r)
       , TableColumn "Pace" "pace" True (\(r,_) -> H.toHtml $ printDuration $ round (pace r))
       , TableColumn "MpH" "mph" True (\(r,_) -> H.toHtml (printf "%.2f" (mph r) :: String))
       , TableColumn "Rest" "rest" True (\(_,m) -> H.toHtml $ show $ metaDaysOff m)
       , TableColumn "Score" "score" True (\(r,_) -> H.toHtml $ show $ round (scoreRun r))
       , TableColumn "Score Rank" "score_rank" True (\(_,m) -> H.toHtml $ metaScoreRank m)
       , TableColumn "Pace Rank" "pace_rank" True (\(_,m) -> H.toHtml $ metaPaceRank m)
       , TableColumn "Miles7" "miles7" True (\(_,m) -> H.toHtml (printf "%.1f" (metaMiles7 m) :: String))
       , TableColumn "Comment" "" False (\(r,_) -> H.toHtml $ runComment r)
       , TableColumn "Edit" "" False (\(r,_) -> editCellHtml (runId r))
       ]

shouldReverse :: String -> Bool -> TableColumn -> Bool
shouldReverse currentSortKey currentReverse col =
  (currentSortKey == (colSortableKey col)) && (not currentReverse)

dataTableHeaderCssClass :: String -> TableColumn -> H.AttributeValue
dataTableHeaderCssClass currentSortKey col =
  toValue $ (if (currentSortKey == (colSortableKey col))
             then "sorted"
             else
               if (colSortable col)
               then "sortable"
               else "" :: String)

dataTableHeaderCell :: String -> Bool -> TableColumn -> H.Html
dataTableHeaderCell currentSort currentReverse col =
  H.td ! A.class_ (dataTableHeaderCssClass currentSort col) $ H.b $ case (colSortable col) of
    True -> H.a ! A.href (toValue ("/rundata?sort_by=" ++ (colSortableKey col) ++ "&sort_reverse=" ++ (show (shouldReverse currentSort currentReverse col)))) $ H.toHtml $ colHumanName col
    False -> H.toHtml $ colHumanName col

dataTableHeader :: String -> Bool ->H.Html
dataTableHeader currentSort currentReverse =
  H.thead $ H.tr $ do
    mconcat $ map (dataTableHeaderCell currentSort currentReverse) cols

dataTableRow :: (Run, RunMeta) -> H.Html
dataTableRow (r,meta) = H.tr $ mapM_ (\col -> H.td $ ((colDataFn col) (r,meta))) cols

runDataHtml :: User -> Maybe Run -> Day -> H.Html
runDataHtml user run today =
  H.html $ do
    headHtml "New run"
    H.body $ do
      headerBarHtml user user
      H.form ! A.class_ "runform" ! A.method "post" ! A.action "/handlemutaterun" $ do
        H.input ! A.type_ "hidden"
                ! A.name "id"
                ! A.value (case run of
                              Just r -> toValue (show (runId r))
                              Nothing -> "0")
        H.table $ do
          mconcat $ map (runDataFormRow run)
                   [ ("Distance", "distance", "text", "", (show . runDistance), [])
                   , ("Time", "time", "text", "", (printDuration . runDuration), [])
                   , ("Incline", "incline", "text", "", (show . runIncline), [])
                   , ("Date", "date", "date", formatTimeForInput today, (formatTimeForInput . runDate), [])
                   , ("Comment", "comment", "text", "", runComment, [
                         (A.size (toValue (75 :: Int)))])
                   ]
        case run of
          Just _ -> do
            H.input ! A.type_ "submit" ! A.name "button" ! A.value (toValue $ show Modify)
            H.input ! A.type_ "submit" ! A.name "button" ! A.value (toValue $ show Delete)
          Nothing -> H.input ! A.type_ "submit" ! A.name "button" ! A.value (toValue $ show Create)


runDataFormRow :: Maybe Run -> (String, String, String, String, (Run -> String), [H.Attribute]) -> H.Html
runDataFormRow mrun (name, userid, formType, defaultValue, extractValue, extraAs) =
  let defaultAs =
        [ A.type_ (toValue formType)
        , A.id (toValue userid)
        , A.name (toValue userid)
        , A.value $ toValue $ fromMaybe defaultValue (fmap extractValue mrun)
        ] in
  H.div ! A.class_ "formitem" $ do
    H.div $ H.label ! A.for (toValue userid) $ H.toHtml name
    H.div $ foldr (flip (!)) H.input (defaultAs ++ extraAs)

notLoggedInHtml :: String -> H.Html
notLoggedInHtml googleUrl =
  H.html $ do
    headHtml "Please log in."
    H.body ! A.class_ "loginpage" $ do
      H.div ! A.class_ "login" $
        H.a ! A.href (toValue googleUrl) $ "Login"

jsArray :: String -> String -> String
jsArray name contents =
  printf "var %s = [%s];" name contents


jsDate :: Day -> String
jsDate d = formatTime defaultTimeLocale "new Date(%Y, (%m - 1), %e)" d

data ChartKind = Line | Scatter deriving (Show)

data Series = Series { seriesLabel :: String
                     , seriesDataFn :: ((Run, RunMeta) -> String)
                     , seriesId :: String
                     }

data Chart = Chart { chartTitle :: String
                   , chartKind :: ChartKind
                   , chartId :: String
                   , chartSerieses :: [Series]
                   }

genId :: Chart -> Series -> String
genId chart series = printf "%s_%s" (chartId chart) (seriesId series)

seriesJs :: Chart -> [(Run, RunMeta)] -> Series -> String
seriesJs chart runs series =
  jsArray (genId chart series) $ concat . intersperse "," $ map (seriesDataFn series) runs

oneChartJs :: Chart -> [(Run, RunMeta)] -> String
oneChartJs chart runs = concat
  ((++)
   (map (seriesJs chart  runs) (chartSerieses chart))
   [ jsArray ((chartId chart) ++  "_dates") $ concat . intersperse "," $ map (jsDate . runDate . fst) runs
   , printf "xyChart('%s', '%s_div' , %s_dates, [%s], [%s]);"
     (show (chartKind chart))
     (chartId chart)
     (chartId chart)
     (concat (intersperse "," (map (genId chart) (chartSerieses chart))))
     (concat (intersperse "," (map (show . seriesLabel) (chartSerieses chart))))
   ])

oneChartHtml :: Chart -> [(Run, RunMeta)] -> H.Html
oneChartHtml chart runs =
  let divname = (chartId chart) ++ "_div"
  in do
    H.h3 $ H.toHtml (chartTitle chart)
    H.div ! A.id (toValue divname) ! A.class_ "chart_div" $ ""
    H.script ! A.type_ "text/javascript" $ H.toHtml $
      oneChartJs chart runs

charts :: [Chart]
charts = [ Chart "Miles per week" Line "mpw_chart" [ Series "MPW" (show . metaMiles7 . snd) "mpw7_series"
                                                   , Series "MPW (last 8w)" (show . metaMiles56 . snd) "mpw56_series"]
         , Chart "Pace (mph)" Scatter "mph_chart" [ Series "Pace (mph)" (show . mph . fst) "pace_series" ]
         , Chart "Score" Scatter "score_chart" [ Series "Score" (show . scoreRun . fst) "score_series" ]
         ]


mpwChartHtml :: [(Run, RunMeta)] -> Integer -> User -> H.Html
mpwChartHtml runs currentLookback user =
  H.html $ do
    headHtml "Charts"
    H.body $ do
      headerBarHtml user user
      H.div ! A.class_ "chart_wrapper" $ do
        H.form ! A.onsubmit "changeLookback(); return false;" $ do
          H.input ! A.value (toValue (show currentLookback)) ! A.id "lookback_days"
          H.input ! A.type_ "submit" ! A.value "Change lookback"
        mapM_ (\c -> oneChartHtml c runs) charts

importFormHtml :: H.Html
importFormHtml =
  H.html $ do
    headHtml "Import data"
    H.body $ do
      H.form ! A.method "post"
             ! A.action "/handleimport"
             ! A.enctype "multipart/form-data" $ do
        H.div $ H.input ! A.type_ "file" ! A.name "filedata"
        H.div $ H.input ! A.type_ "submit"

monthlyDataRow :: MonthlyData -> H.Html
monthlyDataRow md =
  H.tr $ do
    H.td $ H.toHtml $ monthlyYear md
    H.td $ H.toHtml $ monthlyMonth md
    H.td $ H.toHtml $ monthlyMiles md

monthlyDataHtml :: [MonthlyData] -> User -> H.Html
monthlyDataHtml monthlyDataPoints user =
  H.html $ do
    headHtml "Monthly Summary"
    H.body $ do
      headerBarHtml user user
      H.table ! A.class_ "datatable" ! A.style "width: auto" $ do
        H.tr $ do
          H.th $ "Year"
          H.th $ H.a ! A.href "/monthly" $ "Month"
          H.th $ H.a ! A.href "/monthly?sort_by=miles" $ "Miles"
        mapM_ monthlyDataRow monthlyDataPoints
