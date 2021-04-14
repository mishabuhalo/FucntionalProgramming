{-# LANGUAGE OverloadedStrings #-}
module UsersTable where

import qualified Data.Text as T ( Text, pack )
import Database.MySQL.Base
import MySQLConnector
import Data.Int (Int32)
import qualified Text.PrettyPrint as TPrettyP ( ($+$), text, vcat, Doc, (<>), render )
import Converter (mergeLists, myToInt32, myToString, genStruct, genMyLine, genRow )

data UsersInfo = UsersInfo
    { tableName :: String,
      fieldNames :: [String],
      ids :: [Int32],
      emails :: [String],
      passwords :: [String],
      names :: [String]
    }
    deriving Show

emptyUserStruct :: UsersInfo
emptyUserStruct = UsersInfo "users" ["user_id", "email", "password", "name"] [] [] [] []

mergeAll :: UsersInfo -> [[String]]
mergeAll tableInfo = mergeLists
                        (mergeLists
                            (mergeLists
                                (mergeLists [] id (map show (ids tableInfo)) "" maxVal)
                                id (emails tableInfo) "" maxVal)
                            id (passwords tableInfo) "" maxVal)
                        id (names tableInfo) "" maxVal
                    where maxVal = max (length (ids tableInfo)) 
                                        (max (length (emails tableInfo)) 
                                            (max (length (passwords tableInfo))
                                                    (length (names tableInfo))))

instance Table UsersInfo where
    getName tableInfo = tableName tableInfo

    getFieldNames tableInfo =
                        [fieldNames tableInfo !! 0 | not (null (ids tableInfo))] ++
                        [fieldNames tableInfo !! 1 | not (null (emails tableInfo))] ++
                        [fieldNames tableInfo !! 2 | not (null (passwords tableInfo))] ++
                        [fieldNames tableInfo !! 3 | not (null (names tableInfo))]

    getFieldValues (UsersInfo _ _ ids emails passwords names) =
        map MySQLInt32 ids ++
        map (MySQLText . T.pack) emails ++
        map (MySQLText . T.pack) passwords ++
        map (MySQLText . T.pack) names

    getMainFieldTables tableInfo =  UsersInfo {
            tableName = tableName tableInfo,
            fieldNames = fieldNames tableInfo,
            ids = [],
            emails = emails tableInfo,
            passwords = [],
            names = []
        }

    fromMySQLValues res = do
        vals <- res
        return (UsersInfo {
            tableName = tableName emptyUserStruct,
            fieldNames = fieldNames emptyUserStruct,
            ids = map myToInt32 (genStruct vals 0),
            emails = map myToString (genStruct vals 1),
            passwords = map myToString (genStruct vals 2),
            names = map myToString (genStruct vals 3)
        })

    isEmpty tableInfo = null (ids tableInfo) || null (emails tableInfo) ||
                        null (names tableInfo) || null (passwords tableInfo)

    len tableInfo = fromEnum (not (null (ids tableInfo))) +
                    fromEnum (not (null (emails tableInfo))) +
                    fromEnum (not (null (names tableInfo))) +
                    fromEnum (not (null (passwords tableInfo)))

    printInfo tableInfo _ = putStrLn (TPrettyP.render draw ++ "\n")
        where
            draw :: TPrettyP.Doc
            draw = TPrettyP.text header
                TPrettyP.$+$
                    TPrettyP.text (genMyLine (length header +
                                              7 * length (filter (== '\t') header)))
                TPrettyP.$+$
                    TPrettyP.vcat (map row (mergeAll tableInfo))
                where
                    row t = TPrettyP.text( genRow (fieldNames tableInfo) t id)
                    header = genRow (fieldNames tableInfo) (fieldNames tableInfo) id
