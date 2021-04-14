module Converter where


import Data.Text as T ( Text )
import Data.Time.Calendar as C ( Day, fromGregorian )
import Database.MySQL.Base
import qualified Data.ByteString.Lazy.UTF8 as BSU
import Data.Binary.Put (runPut)
import Data.Int (Int32)
import Data.Time (parseTimeOrError, defaultTimeLocale )
import System.Console.Haskeline
    ( defaultSettings, getInputLine, runInputT )

genStruct :: [[MySQLValue]] -> Int -> [MySQLValue]
genStruct xs ind = foldr (\x -> (++) [x !! ind]) [] xs

mergeLists :: [[a]] -> (b -> a) -> [b] -> b -> Int -> [[a]]
mergeLists [] func [] def n
    | n <= 0 = []
    | otherwise = [func def] : mergeLists [] func [] def (n-1)

mergeLists (x:xs) func [] def n = (x ++ [func def]) : mergeLists xs func [] def (n-1)
mergeLists [] func (nx:nxs) def n = [func nx] : mergeLists [] func nxs def (n-1)
mergeLists (lx:lxs) func (nx:nxs) def n = (lx ++ [func nx]) : mergeLists lxs func nxs def (n-1)

myToString :: MySQLValue -> String
myToString val = BSU.toString (runPut (putTextField val))

myToInt32 :: MySQLValue -> Int32
myToInt32 val = strToInt32 (myToString val)

myToDay :: MySQLValue -> C.Day
myToDay val = strToDay (myToString val)

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
                  [(val, "")] -> Just val
                  _           -> Nothing

readStr :: String -> IO String
readStr outValue = do
    content <- runInputT defaultSettings (getInputLine (outValue ++ "> "))
    case content of
        Just s -> return s
        _ -> return ""

readInt :: String -> IO (Maybe Int)
readInt outValue = do
    content <- runInputT defaultSettings (getInputLine (outValue ++ "> "))
    case content of
        Just s -> return (readMaybe s :: Maybe Int)
        _ -> return Nothing

strToInt32 :: String -> Int32
strToInt32 s = case (readMaybe s :: Maybe Int32) of
                Just val -> val
                _        -> 0

strToDay :: String -> C.Day
strToDay = parseTimeOrError True defaultTimeLocale "'%Y-%m-%d'"

strToDayUserInteract :: String -> C.Day
strToDayUserInteract = parseTimeOrError True defaultTimeLocale "%Y-%m-%d"

genRow :: [String] -> [a] -> (a -> String) -> String
genRow [] [] func = ""
genRow [s] [x] func = func x
genRow (colX:colXs) (x:xs) func
    | func x == "user_id" || func x == "author_id" || func x == "resource_id" 
        || func x == "record_id" = "id\t|  " ++ genRow colXs xs func
    | colX == "user_id" || colX == "author_id" || colX == "resource_id" 
        || colX == "record_id" = func x ++ "\t|  " ++ genRow colXs xs func
    | func x == "author" && colX == "author" || func x == "email" && colX == "email" = func x ++ "\t\t|  " ++ genRow colXs xs func
    | func x == "user" && colX == "user" || func x == "link" = func x ++ "\t\t\t|  " ++ genRow colXs xs func
    | func x /= "name" && colX == "name" = func x ++ "\t|  " ++ genRow colXs xs func
    | func x == "name" && colX == "name" = func x ++ "\t\t|  " ++ genRow colXs xs func
    | length (func x) <= 4 && colX /= "type" = func x ++ "\t\t|  " ++ genRow colXs xs func
    | length (func x) <= 8 && colX /= "type" && colX /= "annotation" && colX /= "purpose" && colX /= "open_date" 
            = func x ++ "\t\t|  " ++ genRow colXs xs func
    | length (func x) <= 12 && colX /= "type" && colX /= "annotation" && colX /= "purpose" && colX /= "open_date" && colX /= "usage_time"
            = func x ++ "\t\t|  " ++ genRow colXs xs func
    | otherwise    = func x ++ "\t|  " ++ genRow colXs xs func

genMyLine :: Int -> String
genMyLine n
    | n <= 0 = ""
    | otherwise = "-" ++ genMyLine (n - 1)