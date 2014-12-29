{-# LANGUAGE OverloadedStrings #-}

-- sudo apt-get install libmysqlclient-dev
-- cabal install mysql-simple
-- cabal install happstack
-- cabal install wreq
-- cabal install jwt
-- cabal install cassava

-- sudo iptables -t nat -A PREROUTING -i eth0 -p tcp --dport 80 -j REDIRECT --to-port 8000

module Workouts(WorkoutConf(..), workoutMain, computeRest,rankAsc,parseDuration) where

import Control.Applicative ((<$>), (<*>))
import Control.Lens ((^.), (^..))
import Control.Monad (msum,mzero)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Lazy (State, state, get, runState)
import qualified Data.Aeson as J ((.:))
import Data.Aeson hiding (decode)
import qualified Data.Aeson as JSON (decode, FromJSON(..), Object(..))
import Data.Aeson.Lens (key, _String, values)
import qualified Data.ByteString.Char8 as C8 (pack,unpack,ByteString,length,append)
import qualified Data.ByteString.Lazy.Char8 as C8L (fromStrict)
import qualified Data.ByteString.Lazy as BL (readFile)
import qualified Data.ByteString as BS (unpack)
import qualified Data.ByteString.Base64 as BS64 (decode, decodeLenient)
import qualified Data.Csv as CSV (decodeByName, FromNamedRecord(..), (.:), Parser(..), NamedRecord(..), lookup)
import Data.Int (Int64)
import qualified Data.HashMap.Strict as HM (HashMap(..), keys, lookup)
import Data.List (reverse, sort, findIndex, zip5, intersperse, concat)
import Data.Monoid (mconcat)
import qualified Data.Text as Text (splitOn, pack, unpack, Text)
import qualified Data.Text.Lazy as TL (unpack)
import qualified Data.Text.Encoding as TextEnc (encodeUtf8, decodeUtf8)
import Data.Time.Calendar (Day, fromGregorianValid, fromGregorian, diffDays)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, FormatTime)
import Data.Time.LocalTime (LocalTime, utcToLocalTime, getCurrentTimeZone, localDay)
import qualified Data.Vector as Vector (forM_, forM)
import Database.MySQL.Simple
import Database.MySQL.Simple.QueryResults (QueryResults, convertResults)
import Database.MySQL.Simple.Result (convert)
import Happstack.Server (dir, nullConf, simpleHTTPWithSocket, toResponse, ok, Response, ServerPartT, look, body, decodeBody, defaultBodyPolicy, queryString, seeOther, nullDir, mkCookie, addCookie, readCookieValue, CookieLife(Session), lookCookieValue, expireCookie, withHost, port, bindPort, checkRqM, serveFile, asContentType, lookFile)
import Network.Wreq (post, responseBody, FormParam((:=)))
import System.Locale (defaultTimeLocale)
import Text.Blaze (toValue)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Web.JWT (decode, claims, header, signature)

data WorkoutConf = WorkoutConf { wcGoogleClientId :: String
                               , wcGoogleClientSecret :: String
                               , wcAdminKind :: String
                               , wcAdminId :: String
                               , wcPort :: Int
                               , wcMysqlHost :: String } deriving (Show)   

workoutMain :: WorkoutConf -> IO ()
workoutMain wc = do
  let httpConf = nullConf {  port = wcPort wc }
  socket <- bindPort nullConf { port = wcPort wc }
  simpleHTTPWithSocket socket httpConf $ allPages wc


--
-- Data types
--

data RunMeta = RunMeta { daysOff :: Integer
                       , scoreRank :: Int
                       , paceRank :: Int
                       , miles7 :: Float
                       , miles56 :: Float
                       }

data Run = Run { distance :: Float
               , duration :: Int
               , date :: Day
               , incline :: Float
               , comment :: String
               , runid :: Int
               , runUserId :: Int
               } deriving (Show)

data User = User { userId :: Int,
                   userName :: String,
                   userKind :: String,
                   foreignUserId :: String } deriving (Read, Show)

data GoogleUser = GoogleUser { googleEmail :: String
                             , googleId :: String } deriving (Show)

data MutationKind = Create | Modify | Delete deriving (Read, Show)

data IdentityProvider = Google deriving (Show)

data Identity = Identity{ displayName :: String
                        , uniqueId :: String
                        , provider :: IdentityProvider } deriving (Show)

--
-- Routing / handlers
--

mb :: Int64
mb = 1024 * 1024

allPages :: WorkoutConf -> ServerPartT IO Response
allPages wc =
  withHost (\host -> do
               decodeBody (defaultBodyPolicy "/tmp" (10 * mb) (10 * mb) (10 * mb))
               conn <- liftIO $ dbConnect (wcMysqlHost wc)
               redirectUrl <- return $ "http://" ++ host ++ "/handlelogin"
               msum [ dir "logout" $ logoutPage
                    , dir "js" $ serveFile (asContentType "text/javascript") "static/js/workouts.js"
                    , dir "css" $ serveFile (asContentType "text/css") "static/css/workouts.css"
                    , dir "admin" $ dir "mkdb" $ mkDbPage conn
                    , loggedInPages conn (wcGoogleClientId wc) (wcAdminKind wc) (wcAdminId wc)
                    , dir "handlelogin" $ handleLoginPage conn (wcGoogleClientId wc) (wcGoogleClientSecret wc) redirectUrl
                    , do loginUrl <- return $ googleLoginUrl (wcGoogleClientId wc) redirectUrl ""
                         ok $ toResponse $ notLoggedInHtml loginUrl
                    ])

loggedInPages :: Connection -> String -> String -> String -> ServerPartT IO Response
loggedInPages conn googleClientId adminKind adminId = do
  user <- (readCookieValue "userid") `checkRqM` (userWithId conn)
  msum [ dir "admin" $ dir "refreshdb" $ requireAdmin conn adminKind adminId (refreshDbPage conn)
       , dir "rundata" $ runDataPage conn user
       , dir "editrun" $ editRunFormPage conn user
       , dir "newrun" $ newRunFormPage user
       , dir "handlemutaterun" $ handleMutateRunPage conn user
       , dir "chart" $ dir "mpw" $ mpwChartPage conn user
       , dir "import" $ importFormPage
       , dir "handleimport" $ handleImportPage conn user
       , runDataPage conn user
       ]

importFormPage :: ServerPartT IO Response
importFormPage = ok $ toResponse $ importFormHtml

data CsvRunRecord = CsvRunRecord { csvRunDate :: String
                                 , csvRunDist :: Float
                                 , csvRunDuration :: String
                                 , csvRunIncline :: Maybe Float
                                 , csvRunComment :: Maybe String } deriving (Show)

instance CSV.FromNamedRecord CsvRunRecord where
  parseNamedRecord record =
    CsvRunRecord <$>
    record CSV..: "Date" <*>
    record CSV..: "Distance" <*>
    record CSV..: "Time" <*>
    record CSV..: "Inc" <*>
    record CSV..: "Comment"

maybeWithDefault :: Maybe a -> a -> a
maybeWithDefault mval defaultVal = case mval of
  Just a -> a
  Nothing -> defaultVal

parseCsvRun :: User -> CsvRunRecord -> Maybe Run
parseCsvRun owner csv = do
  date <- parseDateDDMMYYYYslash (csvRunDate csv)
  duration <- parseDuration (csvRunDuration csv)
  inc <- return $ maybeWithDefault (csvRunIncline csv) 0.0
  comment <- return $ maybeWithDefault (csvRunComment csv) ""
  return $ Run (csvRunDist csv) duration date inc comment 0 (userId owner)

handleImportPage :: Connection -> User -> ServerPartT IO Response
handleImportPage conn user = do
  (fname, _, _) <- lookFile "filedata"
  csvData <- liftIO $ BL.readFile fname
  case CSV.decodeByName csvData of
    Left err -> ok $ toResponse $ simpleMessageHtml ("ERROR: " ++ err)
    Right (_, v) -> do
      liftIO $ Vector.forM v (\r -> do
                                 run <- return $ (parseCsvRun user r)
                                 storeRun conn run (Just Create))
      ok $ toResponse $ simpleMessageHtml "foo"

mpwChartPage :: Connection -> User -> ServerPartT IO Response
mpwChartPage conn user = do
  runs <- liftIO $ query conn "SELECT miles, duration_sec, date, incline, comment, id, user_id FROM happstack.runs WHERE user_id = (?) ORDER BY date ASC" [(userId user)]
  annotated <- return $ annotate runs
  ok $ toResponse $ mpwChartHtml annotated user


logoutPage :: ServerPartT IO Response
logoutPage = do
  expireCookie "userid"
  seeOther ("/" :: String) $ toResponse ("Logging out..." :: String)

handleLoginPage :: Connection -> String -> String -> String -> ServerPartT IO Response
handleLoginPage conn clientid secret redirectUrl = do
  code <- look "code"
  mid <- liftIO $ getGoogleId clientid secret code redirectUrl
  case mid of
    Nothing -> ok $ toResponse $ simpleMessageHtml "Login failed"
    Just id -> do
      mu <- liftIO $ findOrInsertGoogleUser conn (displayName id) (uniqueId id)
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
                        
landingPage :: Connection -> String -> ServerPartT IO Response
landingPage conn googleClientId = do
  uid <- readCookieValue "userid"
  mu <- liftIO $ findUserById conn uid
  ok $ toResponse $ landingPageHtml mu


refreshDbPage :: Connection -> ServerPartT IO Response
refreshDbPage conn = do
  drop <- liftIO $ dropTable conn
  runs <- liftIO $ mkRunTable conn
  users <- liftIO $ mkUserTable conn
  ok (toResponse (executeSqlHtml "create table" (drop + runs + users)))

mkDbPage :: Connection -> ServerPartT IO Response
mkDbPage conn = do
  runs <- liftIO $ mkRunTable conn
  users <- liftIO $ mkUserTable conn
  ok (toResponse (executeSqlHtml "create table" (runs + users)))

runDataPage :: Connection -> User -> ServerPartT IO Response
runDataPage conn user = do
  runs <- liftIO $ query conn "SELECT miles, duration_sec, date, incline, comment, id, user_id FROM happstack.runs WHERE user_id = (?)ORDER BY date ASC" [(userId user)]
  ok (toResponse (dataTableHtml user (annotate runs)))

newRunFormPage :: User -> ServerPartT IO Response
newRunFormPage user = do
  tz <- liftIO $ getCurrentTimeZone
  utcNow <- liftIO $ getCurrentTime
  today <- return $ localDay $ utcToLocalTime tz utcNow
  ok $ toResponse $ runDataHtml user Nothing today Create

editRunFormPage :: Connection -> User -> ServerPartT IO Response
editRunFormPage conn user = do
  id <- queryString $ look "id"
  runs <- liftIO $ (query conn "SELECT miles, duration_sec, date, incline, comment, id, user_id FROM happstack.runs WHERE id = (?)" [id])
  ok $ toResponse $ runDataHtml user (Just (head runs)) (fromGregorian 2014 1 1) Modify

handleMutateRunPage :: Connection -> User -> ServerPartT IO Response
handleMutateRunPage conn user= do
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

--
-- Google login flow
--

userWithId :: Connection -> String -> ServerPartT IO (Either String User)
userWithId conn id = do
  maybeuser <- liftIO $ findUserById conn id
  return $ case maybeuser of
    Nothing -> Left ("No user with id: " ++ id)
    Just user -> Right user

googleLoginUrl :: String -> String -> String -> String
googleLoginUrl clientid redirect state =
  printf "https://accounts.google.com/o/oauth2/auth?client_id=%s&response_type=code&scope=openid%%20email&redirect_uri=%s&state=%s" clientid redirect state

getGoogleIdUrl :: String
getGoogleIdUrl = "https://www.googleapis.com/oauth2/v3/token"

data JWTHeader = JWTHeader { alg :: String, kid :: String } deriving (Show)

instance JSON.FromJSON JWTHeader where
  parseJSON (Object o) = JWTHeader <$>
                              o J..: "alg" <*>
                              o J..: "kid"
  parseJSON _ = mzero

data JWTPayload = JWTPayload { iss :: String
                             , at_has :: String
                             , email_verified :: Bool
                             , sub :: String
                             , azp :: String
                             , email :: String
                             , aud :: String
                             , iat :: Int
                             , exp :: Int } deriving (Show)

instance JSON.FromJSON JWTPayload where
  parseJSON (Object o) = JWTPayload <$>
                         o J..: "iss" <*>
                         o J..: "at_hash" <*>
                         o J..: "email_verified" <*>
                         o J..: "sub" <*>
                         o J..: "azp" <*>
                         o J..: "email" <*>
                         o J..: "aud" <*>
                         o J..: "iat" <*>
                         o J..: "exp"

jwtDecode :: FromJSON a => Text.Text -> Maybe a
jwtDecode inTxt =
  JSON.decode (C8L.fromStrict (BS64.decodeLenient (TextEnc.encodeUtf8 inTxt)))

decodeToString :: Text.Text -> String
decodeToString inTxt = 
  Text.unpack (TextEnc.decodeUtf8 (BS64.decodeLenient (TextEnc.encodeUtf8 inTxt)))
  
getGoogleId :: String -> String -> String -> String -> IO (Maybe Identity)
getGoogleId clientid secret code redirectUrl = do
  r <- post getGoogleIdUrl
       [ "code" := code
       , "client_id" := clientid
       , "client_secret" := secret
       , "redirect_uri" := redirectUrl
       , "grant_type" := ("authorization_code" :: String)
       ]
  encodedToken <- return $ r ^. responseBody .key "id_token" . _String
  encodedParts <- return $ Text.splitOn "." encodedToken
  -- TODO(mrjones): verify the signature with the algorithm named in
  -- the header
  -- jwtHeader <- return $ (jwtDecode (head encodedParts) :: Maybe JWTHeader)
  jwtPayload <- return $ (jwtDecode (head (tail encodedParts)) :: Maybe JWTPayload)
  return $ case jwtPayload of
    Nothing -> Nothing
    Just payload -> Just (Identity (email payload) (sub payload) Google)

findOrInsertGoogleUser :: Connection -> String -> String -> IO (Maybe User)
findOrInsertGoogleUser conn email sub = do
  fromDb <- findGoogleUser conn sub
  case fromDb of
    Nothing -> do n <- insertGoogleUser conn email sub
                  findGoogleUser conn sub
    Just u -> return $ Just u


findUserById :: Connection -> String -> IO (Maybe User)
findUserById conn id = do
  users <- query conn "SELECT id, display, kind, foreign_id FROM happstack.users WHERE id = (?)" [id]
  case users of
    [] -> return $ Nothing
    (u:_) -> return $ Just u

findGoogleUser :: Connection -> String -> IO (Maybe User)
findGoogleUser conn sub = do
  users <- query conn "SELECT id, display, kind, foreign_id FROM happstack.users WHERE kind = 'google' AND foreign_id = (?)" [sub]
  case users of
    [] -> return $ Nothing
    (u:_) -> return $ Just u


insertGoogleUser :: Connection -> String -> String -> IO Int64
insertGoogleUser conn email sub =
    execute conn "INSERT INTO happstack.users (display, kind, foreign_id) VALUES (?, 'google', ?)" (email, sub)

--
-- Misc application logic
--

annotate :: [Run] -> [(Run, RunMeta)]
annotate rs = zip rs (annotate2 rs)

annotate2 :: [Run] -> [RunMeta]
annotate2 rs = map buildMeta
               (zip5
                (computeRest (map date rs))
                (rankDesc (map scoreRun rs))
                (rankAsc (map pace rs))
                (trailingMileage 7 7 rs)
                (trailingMileage 56 7 rs))

buildMeta :: (Integer, Maybe Int, Maybe Int, Float, Float) -> RunMeta
buildMeta (rest, mscore, mpace, miles7, miles56) =
  let score = case mscore of
        Just s -> s + 1
        Nothing -> 0
      pace = case mpace of
        Just p -> p + 1
        Nothing -> 0
  in RunMeta rest score pace miles7 miles56


trailingOne :: Integer -> Integer -> Run -> State [Run] Float
trailingOne windowSize denominatorDays nextRun =
  let scale = (fromIntegral denominatorDays) / (fromIntegral windowSize)
  in state $ (\rs -> (foldr (\candidate (distAcc, outAcc) ->
                              if (diffDays (date nextRun) (date candidate) < windowSize)
                              then ((distAcc + (scale * (distance candidate))), candidate:outAcc)
                              else (distAcc, outAcc)) (0.0, []) (rs ++ [nextRun])))

trailingAll :: Integer -> Integer -> [Run] -> State [Run] [Float]
trailingAll windowSize denominatorDays runs =
  mapM (trailingOne windowSize denominatorDays) runs

trailingMileage :: Integer -> Integer -> [Run] -> [Float]
trailingMileage windowSize denominatorDays runs =
  fst $ runState (trailingAll windowSize denominatorDays runs) []  

computeRest :: [Day] -> [Integer]
computeRest ds =
  let shifted = take (length ds) ((head ds):ds) :: [Day]
  in map (uncurry diffDays) (zip ds shifted)

rankAsc :: Ord a => [a] -> [Maybe Int]
rankAsc = rank id

rankDesc :: Ord a => [a] -> [Maybe Int]
rankDesc = rank reverse

rank :: Ord a => ([a] -> [a]) -> [a] -> [Maybe Int]
rank order ins =
  let sorted = order (sort ins)
  in map (\x -> findIndex ((==) x) sorted) ins

-- 1000 * 4 * (distance^1.06)/(time_minutes)
scoreRun :: Run -> Float
scoreRun r =
  let time_minutes = (fromIntegral (duration r)) / 60
  in 1000 * 4 * ((distance r) ** (1.06)) / time_minutes

parseRun :: String -> String -> String -> String -> String -> String -> Int -> Maybe Run
parseRun distanceS durationS dateS inclineS commentS idS userId = do
  distance <- readMaybe distanceS :: Maybe Float
  incline <- readMaybe inclineS :: Maybe Float
  duration <- parseDuration durationS
  date <- parseDateYYYYMMDDhyph dateS
  id <- readMaybe idS :: Maybe Int
  Just (Run distance duration date incline commentS id userId)

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
  min <- readMaybe (Text.unpack minS) :: Maybe Int
  sec <- readMaybe (Text.unpack secS) :: Maybe Int
  Just (hour * 3600 + min * 60 + sec)

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

parseDateDDMMYYYYslash :: String -> Maybe Day
parseDateDDMMYYYYslash input = do
  parts <- Just (Text.splitOn (Text.pack "/") (Text.pack input))
  case parts of
    [monthS,dayS,yearS] -> do
      year <- readMaybe (Text.unpack yearS) :: Maybe Integer
      month <- readMaybe (Text.unpack monthS) :: Maybe Int
      day <- readMaybe (Text.unpack dayS) :: Maybe Int
      fromGregorianValid year month day
    _ -> Nothing

formatTimeForInput :: FormatTime t => t -> String
formatTimeForInput time = formatTime defaultTimeLocale "%Y-%m-%d" time

pace :: Run -> Float
pace r = (fromIntegral (duration r)) / (distance r)

mph :: Run -> Float
mph r = 60 * 60 * (distance r) / (fromIntegral (duration r))

printDuration :: Int -> String
printDuration secs = printf "%d:%02d" (div secs 60) (mod secs 60)

-- TODO(mrjones): push the maybes up to a higher level
storeRun :: Connection -> Maybe Run -> Maybe MutationKind -> IO Int64
storeRun conn mrun mkind =
  case mrun of
    Just run -> case mkind of
      Just kind -> storeRun2 conn run kind
      Nothing -> return 0
    Nothing -> return 0

--
-- Database logic
--

dbConnect :: String -> IO Connection
dbConnect hostname = connect defaultConnectInfo
    { connectUser = "happstack"
    , connectPassword = "happstack"
    , connectDatabase = "happstack"
    , connectHost = hostname
    }

instance QueryResults Run where
  convertResults [f_dist,f_dur,f_date,f_incl,f_comm,f_id,f_uid] [v_dist,v_dur,v_date,v_incl,v_comm,v_id,v_uid] =
    Run (convert f_dist v_dist) (convert f_dur v_dur) (convert f_date v_date) (convert f_incl v_incl) (convert f_comm v_comm) (convert f_id v_id) (convert f_uid v_uid)

instance QueryResults GoogleUser where
  convertResults [f_gemail, f_gid] [v_gemail, v_gid] =
    GoogleUser (convert f_gemail v_gemail) (convert f_gid v_gid)

instance QueryResults User where
  convertResults [f_id, f_disp, f_kind, f_fid] [v_id, v_disp, v_kind, v_fid] =
    User (convert f_id v_id) (convert f_disp v_disp) (convert f_kind v_kind) (convert f_fid v_fid)

storeRun2 :: Connection -> Run -> MutationKind -> IO Int64
storeRun2 conn r kind =
    case kind of
      Create -> execute conn
                "INSERT INTO happstack.runs (date, miles, duration_sec, incline, comment, user_id) VALUES (?, ?, ?, ?, ?, ?)"
                (date r, distance r, duration r, incline r, comment r, runUserId r)
      Modify -> execute conn
                "UPDATE happstack.runs SET date=?, miles=?, duration_sec=?, incline=?, comment=?, user_id=? WHERE id=?"
                (date r, distance r, duration r, incline r, comment r, runUserId r, runid r)
      Delete -> execute conn
                "DELETE FROM happstack.runs WHERE id = (?)"
                [runid r]

--
-- Database Admin
--

dropTable :: Connection -> IO Int64
dropTable conn = do
  execute conn "DROP TABLE happstack.runs" ()
  execute conn "DROP TABLE happstack.users;" ()

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

headerBarHtml :: User -> H.Html
headerBarHtml user =
  H.div ! A.class_ "header" $ do
    H.div ! A.class_ "username" $ H.toHtml $ userName user
    H.a ! A.href "/logout" $ "Logout"
    H.a ! A.href "/newrun" $ "+ New run"
    H.a ! A.href "/rundata" $ "All runs"
    H.a ! A.href "/chart/mpw" $ "Charts"

landingPageHtml :: Maybe User -> H.Html
landingPageHtml muser =
  H.html $ do
    headHtml "Workout database"
    H.body $ case muser of
        Just user -> do
          H.div $ H.toHtml $ userName user
          H.div $ H.a ! A.href "/newrun" $ H.html "New run"
          H.div $ H.a ! A.href "/rundata" $ H.html "View runs"
          H.div $ H.a ! A.href "/logout" $ H.html "Logout"
        Nothing -> H.div $ H.html "Error"

dataTableHtml :: User -> [(Run, RunMeta)] -> H.Html
dataTableHtml u rs =
  H.html $ do
    headHtml "Run data"
    H.body $ do
      headerBarHtml u
      H.table ! A.class_ "datatable" $ do
        dataTableHeader
        mapM_ dataTableRow (reverse rs)
      H.div ! A.id "chart_div" $ ""

dataTableHeader :: H.Html
dataTableHeader =
  H.thead $ H.tr $ do
    mconcat $ map (H.td . H.b)
      ["Date", "Day", "Dist", "Time", "Incline", "Pace", "MpH", "Rest", "Score", "Score Rank", "Pace Rank", "Miles7", "Comment", "Edit"]

dataTableRow :: (Run, RunMeta) -> H.Html
dataTableRow (r,meta) = H.tr $ do
  H.td $ H.toHtml $ formatTime defaultTimeLocale "%Y-%b-%d" (date r)
  H.td $ H.toHtml $ formatTime defaultTimeLocale "%a" (date r)
  H.td $ H.toHtml $ show $ distance r
  H.td $ H.toHtml $ printDuration $ duration r
  H.td $ H.toHtml $ show $ incline r
  H.td $ H.toHtml $ printDuration $ round (pace r)
  H.td $ H.toHtml (printf "%.2f" (mph r) :: String)
  H.td $ H.toHtml $ show $ daysOff meta
  H.td $ H.toHtml $ show $ round (scoreRun r)
  H.td $ H.toHtml $ show $ scoreRank meta
  H.td $ H.toHtml $ show $ paceRank meta
  H.td $ H.toHtml (printf "%.1f" (miles7 meta) :: String)
  H.td $ H.toHtml $ comment r
  H.td $ do
    "["
    H.a ! A.href (toValue ("/editrun?id=" ++ (show (runid r)))) $ "Edit"
    "]"

runDataHtml :: User -> Maybe Run -> Day -> MutationKind -> H.Html
runDataHtml user run today mutationKind =
  H.html $ do
    headHtml "New run"
    H.body $ do
      headerBarHtml user
      H.form ! A.class_ "runform" ! A.method "post" ! A.action "/handlemutaterun" $ do
        H.input ! A.type_ "hidden"
                ! A.name "id"
                ! A.value (case run of
                              Just r -> toValue (show (runid r))
                              Nothing -> "0")
        H.table $ do
          mconcat $ map (runDataFormRow run)
                   [ ("Distance", "distance", "text", "", (show . distance), [])
                   , ("Time", "time", "text", "", (printDuration . duration), [])
                   , ("Incline", "incline", "text", "", (show . incline), [])
                   , ("Date", "date", "date", formatTimeForInput today, (formatTimeForInput . date), [])
                   , ("Comment", "comment", "text", "", comment, [
                         (A.size (toValue (75 :: Int)))])
                   ]
        case run of
          Just _ -> do
            H.input ! A.type_ "submit" ! A.name "button" ! A.value (toValue $ show Modify)
            H.input ! A.type_ "submit" ! A.name "button" ! A.value (toValue $ show Delete)
          Nothing -> H.input ! A.type_ "submit" ! A.name "button" ! A.value (toValue $ show Create)


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
  H.div ! A.class_ "formitem" $ do
    H.div $ H.label ! A.for (toValue id) $ H.toHtml name
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

jsStr :: Show a => a -> String
jsStr d = printf "\"%s\"" (show d)

jsDate :: Day -> String
jsDate date = formatTime defaultTimeLocale "new Date(%Y, (%m - 1), %e)" date

data ChartKind = Line | Scatter deriving (Show)

data Series = Series { seriesLabel :: String
                     , seriesDataFn :: ((Run, RunMeta) -> String)
                     , seriesId :: String
                     }

data Chart = Chart { chartSerieses :: [Series]
                   , chartTitle :: String
                   , chartKind :: ChartKind
                   , chartId :: String
                   }

genId :: Chart -> Series -> String
genId chart series = printf "%s_%s" (chartId chart) (seriesId series)

seriesJs :: Chart -> [(Run, RunMeta)] -> Series -> String
seriesJs chart runs series =
  jsArray (genId chart series) $ concat . intersperse "," $ map (seriesDataFn series) runs

chartJs2 :: Chart -> [(Run, RunMeta)] -> String
chartJs2 chart runs = concat
  ((++)
   (map (seriesJs chart  runs) (chartSerieses chart))
   [ jsArray ((chartId chart) ++  "_dates") $ concat . intersperse "," $ map (jsDate . date . fst) runs
   , printf "xyChart('%s', '%s_div' , %s_dates, [%s], [%s]);"
     (show (chartKind chart))
     (chartId chart)
     (chartId chart)
     (concat (intersperse "," (map (genId chart) (chartSerieses chart))))
     (concat (intersperse "," (map (show . seriesLabel) (chartSerieses chart))))
   ])

chartHtml2 :: Chart -> [(Run, RunMeta)] -> H.Html
chartHtml2 chart runs =
  let divname = (chartId chart) ++ "_div"
  in do
    H.h3 $ H.toHtml (chartTitle chart)
    H.div ! A.id (toValue divname) $ ""
    H.script ! A.type_ "text/javascript" $ H.toHtml $
      chartJs2 chart runs

mpwChartHtml :: [(Run, RunMeta)] -> User -> H.Html
mpwChartHtml runs user =
  H.html $ do
    headHtml "Charts"
    H.body $ do
      headerBarHtml user
      chartHtml2 (Chart [ Series "MPW" (show . miles7 . snd) "7"
                        , Series "MPW (last 8w)" (show . miles56 . snd) "56"] "Miles per week" Line "mpw7") runs
      chartHtml2 (Chart [Series "Pace (mph)" (show . mph . fst) "pace"] "Pace (mph)" Scatter "mph") runs

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
