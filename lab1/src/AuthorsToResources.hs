{-# LANGUAGE OverloadedStrings #-}
module AuthorsToResources where

import qualified Data.Text as T ( Text, pack )
import Database.MySQL.Base
import MySQLConnector
import Data.Int (Int32)
import qualified Text.PrettyPrint as TPrettyP ( ($+$), text, vcat, Doc, (<>), render )
import Converter (mergeLists, myToInt32, myToString, genStruct, genMyLine, genRow )

import qualified AuthorsTable as AT
import qualified ResourcesTable as RT


data AuthorToResources = AuthorToResources
    { tableName :: String,
      fieldNames :: [String],
      ids :: [Int32],
      authorIds :: [Int32],
      resourceIds :: [Int32]
    }
    deriving Show

emptyAuthorToResourceStruct :: AuthorToResources
emptyAuthorToResourceStruct =
    AuthorToResources "author_owns" ["record_id", "author", "resource"] [] [] []

getAuthorEmail :: Int32 -> MySQLConn -> IO String
getAuthorEmail value conn = do
    res <- getValue (AT.AuthorsInfo  {
            AT.tableName = AT.tableName AT.emptyAuthorStruct,
            AT.fieldNames = AT.fieldNames AT.emptyAuthorStruct,
            AT.ids = [value],
            AT.names = [],
            AT.surnames = [],
            AT.emails = []
    }) conn
    return (head (AT.emails res))

getAuthorId :: String -> MySQLConn -> IO Int32
getAuthorId value conn = do
    res <- getValue (AT.AuthorsInfo  {
            AT.tableName = AT.tableName AT.emptyAuthorStruct,
            AT.fieldNames = AT.fieldNames AT.emptyAuthorStruct,
            AT.ids = [],
            AT.names = [],
            AT.surnames = [],
            AT.emails = [value]
    }) conn
    if not (null (AT.ids res))
        then return (head (AT.ids res))
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

getResourceId :: String -> MySQLConn -> IO Int32
getResourceId value conn = do
    res <- getValue (RT.ResourcesInfo {
            RT.tableName = RT.tableName RT.emptyResourceStruct,
            RT.fieldNames = RT.fieldNames RT.emptyResourceStruct,
            RT.ids = [],
            RT.names = [value],
            RT.types = [],
            RT.annotations = [],
            RT.links = [],
            RT.purposes = [],
            RT.openDate = [],
            RT.usageTime = [],
            RT.rules = [],
            RT.statistics = []
    }) conn
    if not (null (RT.ids res))
        then return (head (RT.ids res))
        else return (-1)

listIdsToListVals :: [Int32] -> MySQLConn -> (Int32 -> MySQLConn -> IO String) -> IO [String]
listIdsToListVals [] conn func = return []
listIdsToListVals (x:xs) conn func = do
    res <- listIdsToListVals xs conn func
    newValue <- func x conn
    return (newValue : res)

mergeAll :: AuthorToResources -> MySQLConn -> IO [[String]]
mergeAll tableInfo conn = do 
        emailList <- listIdsToListVals (authorIds tableInfo) conn getAuthorEmail
        resourceNameList <- listIdsToListVals (resourceIds tableInfo) conn getResourceName
        return (mergeLists
                (mergeLists
                    (mergeLists [] id (map show (ids tableInfo)) "" maxVal)
                    id emailList "" maxVal)
                id resourceNameList "" maxVal)
                where maxVal = max (length (ids tableInfo))
                                    (max (length (authorIds tableInfo))
                                        (length (resourceIds tableInfo)))

instance Table AuthorToResources where
    getName tableInfo = tableName tableInfo

    getFieldNames tableInfo =
                        [fieldNames tableInfo !! 0 | not (null (ids tableInfo))] ++
                        [fieldNames tableInfo !! 1 | not (null (authorIds tableInfo))] ++
                        [fieldNames tableInfo !! 2 | not (null (resourceIds tableInfo))]

    getFieldValues (AuthorToResources _ _ ids authorIds resourceIds) =
        map MySQLInt32 ids ++
        map MySQLInt32 authorIds ++
        map MySQLInt32 resourceIds

    getMainFieldTables tableInfo =  AuthorToResources {
            tableName = tableName tableInfo,
            fieldNames = fieldNames tableInfo,
            ids = [],
            authorIds = authorIds tableInfo,
            resourceIds = resourceIds tableInfo
        }

    fromMySQLValues res = do
        vals <- res
        return (AuthorToResources {
            tableName = tableName emptyAuthorToResourceStruct,
            fieldNames = fieldNames emptyAuthorToResourceStruct,
            ids = map myToInt32 (genStruct vals 0),
            authorIds = map myToInt32 (genStruct vals 1),
            resourceIds = map myToInt32 (genStruct vals 2)
        })

    isEmpty tableInfo = null (ids tableInfo) || null (authorIds tableInfo) || null (resourceIds tableInfo)

    len tableInfo = fromEnum (not (null (ids tableInfo))) +
                    fromEnum (not (null (authorIds tableInfo))) +
                    fromEnum (not (null (resourceIds tableInfo)))

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
