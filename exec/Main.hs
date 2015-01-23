import System.Environment (getArgs)
import Workouts(WorkoutConf(..), workoutMain)
import Text.Read (readMaybe)

main:: IO()
main = do
  args <- getArgs
  (googleClientId:googleClientSecret:adminKind:adminId:portS:mysqlHost:staticDir:_)  <- return $ args
  putStrLn $ "Using google client id: " ++ googleClientId
  putStrLn $ "Using google client secret: " ++ googleClientSecret
  putStrLn $ "Using admin kind: " ++ adminKind
  putStrLn $ "Using admin id: " ++ adminId
  putStrLn $ "Using port: " ++ (show portS)
  putStrLn $ "Using mysql host: " ++ mysqlHost
  putStrLn $ "Using static dir: " ++ staticDir
  case (readMaybe portS :: Maybe Int) of
    Nothing -> fail "Couldn't parse port"
    Just p -> do
      workoutMain $ WorkoutConf googleClientId googleClientSecret adminKind adminId p mysqlHost staticDir


