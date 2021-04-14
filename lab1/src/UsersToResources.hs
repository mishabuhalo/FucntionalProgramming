{-# LANGUAGE OverloadedStrings #-}
module UsersToResources where

import qualified Data.Text as T ( Text, pack )
import qualified Data.Time.Calendar as C ( Day )
import Database.MySQL.Base
import MySQLConnector
import Data.Int (Int32)
import qualified Text.PrettyPrint as TPrettyP ( ($+$), text, vcat, Doc, (<>), render )
import Converter (mergeLists, myToInt32, myToString, genStruct, genMyLine, genRow, myToDay )

import qualified UsersTable as UT
import qualified ResourcesTable as RT


data UserToResources = UserToResources
    { tableName :: String,
      fieldNames :: [String],
      ids :: [Int32],
      userIds :: [Int32],
      resourceIds :: [Int32],
      usageStart :: [C.Day]
    }
    deriving Show

emptyUserToResourceStruct :: UserToResources
emptyUserToResourceStruct =
    UserToResources "user_owns" ["record_id", "user", "resource", "usage_start"] [] [] [] []

getUserEmail :: Int32 -> MySQLConn -> IO String
getUserEmail value conn = do
    res <- getValue (UT.UsersInfo {
            UT.tableName = UT.tableName UT.emptyUserStruct,
            UT.fieldNames = UT.fieldNames UT.emptyUserStruct,
            UT.ids = [value],
            UT.emails = [],
            UT.passwords = [],
            UT.names = []
    }) conn
    return (head (UT.emails res))

getUserId :: String -> MySQLConn -> IO Int32
getUserId value conn = do
    res <- getValue (UT.UsersInfo {
            UT.tableName = UT.tableName UT.emptyUserStruct,
            UT.fieldNames = UT.fieldNames UT.emptyUserStruct,
            UT.ids = [],
            UT.emails = [value],
            UT.passwords = [],
            UT.names = []
    }) conn
    if not (null (UT.ids res))
        then return (head (UT.ids res))
        else return (-1)

getResourceName :: Int32 -> MySQLConn -> IO String
getResourceName value conn = do
    res <- getValue (RT.ResourcesInfo {
            RT.tableName = RT.tableName RT.emptyResourceStruct,
            RT.fieldNames = RT.fieldNames RT.emptyResourceStruct,
            RT.ids = [value],
            RT.names = [],
            RT.types = [],
            RT.annotations = [],
            RT.links = [],
            RT.purposes = [],
            RT.openDate = [],
            RT.usageTime = [],
            RT.rules = [],
            RT.statistics = []
    }) conn
    return (head (RT.names res))

listIdsToListVals :: [Int32] -> MySQLConn -> (Int32 -> MySQLConn -> IO String) -> IO [String]
listIdsToListVals [] _ _ = return []
listIdsToListVals (x:xs) conn func = do
    res <- listIdsToListVals xs conn func
    newValue <- func x conn
    return (newValue : res)

mergeAll :: UserToResources -> MySQLConn -> IO [[String]]
mergeAll tableInfo conn = do 
        emailList <- listIdsToListVals (userIds tableInfo) conn getUserEmail
        resourceNameList <- listIdsToListVals (resourceIds tableInfo) conn getResourceName
        return (mergeLists
                    (mergeLists
                        (mergeLists
                            (mergeLists [] id (map show (ids tableInfo)) "" maxVal)
                            id emailList "" maxVal)
                        id resourceNameList "" maxVal)
                    id (map show (usageStart tableInfo)) "" maxVal )
                where maxVal = max (length (ids tableInfo))
                                    (max (length (userIds tableInfo))
                                        (max (length (resourceIds tableInfo))
                                              (length (usageStart tableInfo))))

instance Table UserToResources where
    getName tableInfo = tableName tableInfo

    getFieldNames tableInfo =
                        [fieldNames tableInfo !! 0 | not (null (ids tableInfo))] ++
                        [fieldNames tableInfo !! 1 | not (null (userIds tableInfo))] ++
                        [fieldNames tableInfo !! 2 | not (null (resourceIds tableInfo))] ++ 
                        [fieldNames tableInfo !! 3 | not (null (usageStart tableInfo))]

    getFieldValues (UserToResources _ _ ids userIds resourceIds usageStart) =
        map MySQLInt32 ids ++
        map MySQLInt32 userIds ++
        map MySQLInt32 resourceIds ++
        map MySQLDate usageStart

    getMainFieldTables tableInfo =  UserToResources {
            tableName = tableName tableInfo,
            fieldNames = fieldNames tableInfo,
            ids = [],
            userIds = userIds tableInfo,
            resourceIds = resourceIds tableInfo,
            usageStart = []
        }

    fromMySQLValues res = do
        vals <- res
        return (UserToResources {
            tableName = tableName emptyUserToResourceStruct,
            fieldNames = fieldNames emptyUserToResourceStruct,
            ids = map myToInt32 (genStruct vals 0),
            userIds = map myToInt32 (genStruct vals 1),
            resourceIds = map myToInt32 (genStruct vals 2),
            usageStart = map myToDay (genStruct vals 3)
        })

    isEmpty tableInfo = null (ids tableInfo) || null (userIds tableInfo) || 
                        null (resourceIds tableInfo) || null (usageStart tableInfo)

    len tableInfo = fromEnum (not (null (ids tableInfo))) +
                    fromEnum (not (null (userIds tableInfo))) +
                    fromEnum (not (null (resourceIds tableInfo))) +
                    fromEnum (not (null (usageStart tableInfo)))

    printInfo tableInfo conn = do
        merged <- mergeAll tableInfo conn
        putStrLn (TPrettyP.render (draw merged) ++ "\n")
        where
            draw ::[[String]] -> TPrettyP.Doc
            draw allRows = TPrettyP.text header
                TPrettyP.$+$
                    TPrettyP.text (genMyLine (length header +
                                              7 * length (filter (== '\t') header)))
                TPrettyP.$+$
                    TPrettyP.vcat (map row allRows)
                where
                    row t = TPrettyP.text( genRow (fieldNames tableInfo) t id)
                    header = genRow (fieldNames tableInfo) (fieldNames tableInfo) id
