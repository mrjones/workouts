import Data.Maybe (fromMaybe)
import System.Console.GetOpt (ArgDescr(..), ArgOrder(..), getOpt, OptDescr(..), usageInfo)
import System.Environment (getArgs)
import System.IO (hSetBuffering, BufferMode(LineBuffering), stdout)
import Workouts(WorkoutConf(..), workoutMain)
import Text.Read (readMaybe)

data Flag = GoogleClientIdFlag String
          | GoogleClientSecretFlag String
          | AdminKindFlag String
          | AdminIdFlag String
          | PortFlag Int
          | MySqlHostFlag String
          | MySqlUsernameFlag String
          | MySqlPasswordFlag String
          | StaticDirFlag String
          deriving (Show)

flagDefs :: [OptDescr Flag]
flagDefs =
  [ Option ['i'] ["google_client_id"] (ReqArg GoogleClientIdFlag "ID") "Google client ID (for OpenID)"
  , Option ['s'] ["google_client_secret"] (ReqArg GoogleClientSecretFlag "SECRET") "Google client secret (for Open ID)"
  , Option ['k'] ["admin_kind"] (ReqArg AdminKindFlag "KIND") "OpenID provider (e.g. 'google') for the admin user."
  , Option ['u'] ["admin_id"] (ReqArg AdminIdFlag "ID") "OpenID user id for the admin user."
  , Option ['p'] ["port"] (ReqArg parsePortFlag "PORT") "HTTP Port"
  , Option ['m'] ["mysql_host"] (ReqArg MySqlHostFlag "HOST") "MySQL hostname"
  , Option [] ["mysql_username"] (ReqArg MySqlUsernameFlag "USER") "MySQL username"
  , Option [] ["mysql_password"] (ReqArg MySqlPasswordFlag "PW") "MySQL password"
  , Option ['d'] ["static_dir"] (ReqArg StaticDirFlag "DIR") "Static files root directory"
  ]

parsePortFlag :: String -> Flag
parsePortFlag ps = PortFlag $ fromMaybe 8000 $ readMaybe ps

argvToFlags :: [String] -> IO [Flag]
argvToFlags argv =
  case getOpt Permute flagDefs argv of
    (f,_,[]  ) -> return f
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header flagDefs))
  where header = "Usage: WorkoutsMain [OPTION...]"

flagsToConfig :: [Flag] -> WorkoutConf
flagsToConfig fs =
  foldr (\f c -> case f of
            GoogleClientIdFlag i -> c { wcGoogleClientId = i }
            GoogleClientSecretFlag s -> c { wcGoogleClientSecret = s }
            AdminKindFlag k -> c { wcAdminKind = k }
            AdminIdFlag i -> c { wcAdminId = i }
            PortFlag p -> c { wcPort = p }
            MySqlHostFlag h -> c { wcMysqlHost = h }
            MySqlUsernameFlag u -> c { wcMysqlUser = u }
            MySqlPasswordFlag p -> c { wcMysqlPassword = p }
            StaticDirFlag d -> c { wcStaticDir = d }
        ) defaultConfig fs

defaultConfig :: WorkoutConf
defaultConfig = WorkoutConf "" "" "google" "" 8000 "localhost" "happstack" "happstack" "static"


main:: IO()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "-----"
  argv <- getArgs
  flags <- argvToFlags argv
  putStrLn $ "Flags: " ++ show flags
  workoutMain (flagsToConfig flags)
