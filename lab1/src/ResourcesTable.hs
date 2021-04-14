{-# LANGUAGE OverloadedStrings #-}
module ResourcesTable where

import qualified Data.Text as T ( Text, pack )
import Database.MySQL.Base
import Data.Time.Calendar as C ( Day )
import MySQLConnector
import Data.Int (Int32)
import qualified Text.PrettyPrint as TPrettyP ( ($+$), text, vcat, Doc, (<>), render )
import Converter (mergeLists, myToInt32, myToString, myToDay, genStruct, genMyLine, genRow )

data ResourcesInfo = ResourcesInfo
    { tableName :: String,
      fieldNames :: [String],
      ids :: [Int32],
      names :: [String],
      types :: [String],
      annotations :: [String],
      links :: [String],
      purposes :: [String],
      openDate :: [C.Day],
      usageTime :: [Int32],
      rules :: [String],
      statistics :: [Int32]
    }
    deriving Show

emptyResourceStruct :: ResourcesInfo
emptyResourceStruct = ResourcesInfo "resources" 
    ["resource_id", "name", "type", "annotation", "link", "purpose", 
        "open_date", "usage_time", "rules", "statistics"] 
            [] [] [] [] [] [] [] [] [] []

mergeAll :: ResourcesInfo -> [[String]]
mergeAll tableInfo = mergeLists
    (mergeLists
      (mergeLists
        (mergeLists
          (mergeLists
            (mergeLists
              (mergeLists
                (mergeLists
                  (mergeLists
                    (mergeLists [] id (map show (ids tableInfo)) "" maxVal)
                    id (names tableInfo) "" maxVal)
                  id (types tableInfo) "" maxVal)
                id (annotations tableInfo) "" maxVal)
              id (links tableInfo) "" maxVal)
            id (purposes tableInfo) "" maxVal)
          id (map show (openDate tableInfo)) "" maxVal)
        id (map show (usageTime tableInfo)) "" maxVal)
      id (rules tableInfo) "" maxVal)
    id (map show (statistics tableInfo)) "" maxVal
    where maxVal = max (length (ids tableInfo)) 
            (max (length (names tableInfo)) 
              (max (length (types tableInfo))
                (max (length (annotations tableInfo))
                  (max (length (links tableInfo))
                    (max (length (types tableInfo))
                      (max (length (purposes tableInfo))
                        (max (length (openDate tableInfo))
                          (max (length (usageTime tableInfo))
                            (max (length (rules tableInfo))
                                 (length (statistics tableInfo)))))))))))
                    

instance Table ResourcesInfo where
    getName tableInfo = tableName tableInfo

    getFieldNames tableInfo =
                        [fieldNames tableInfo !! 0 | not (null (ids tableInfo))] ++
                        [fieldNames tableInfo !! 1 | not (null (names tableInfo))] ++
                        [fieldNames tableInfo !! 2 | not (null (types tableInfo))] ++
                        [fieldNames tableInfo !! 3 | not (null (annotations tableInfo))] ++
                        [fieldNames tableInfo !! 4 | not (null (links tableInfo))] ++
                        [fieldNames tableInfo !! 5 | not (null (purposes tableInfo))] ++
                        [fieldNames tableInfo !! 6 | not (null (openDate tableInfo))] ++
                        [fieldNames tableInfo !! 7 | not (null (usageTime tableInfo))] ++
                        [fieldNames tableInfo !! 8 | not (null (rules tableInfo))] ++
                        [fieldNames tableInfo !! 9 | not (null (statistics tableInfo))]

    getFieldValues (ResourcesInfo _ _ ids names typeId annotation link purpose 
            openDate usageTime rules statistics) =
        map MySQLInt32 ids ++
        map (MySQLText . T.pack) names ++
        map (MySQLText . T.pack) typeId ++
        map (MySQLText . T.pack) annotation ++
        map (MySQLText . T.pack) link ++
        map (MySQLText . T.pack) purpose ++
        map MySQLDate openDate ++
        map MySQLInt32 usageTime ++
        map (MySQLText . T.pack) rules ++
        map MySQLInt32 statistics

    getMainFieldTables tableInfo = ResourcesInfo {
            tableName = tableName tableInfo,
            fieldNames = fieldNames tableInfo,
            ids = [],
            names = names tableInfo,
            types = [],
            annotations = [],
            links = [],
            purposes = [],
            openDate = [],
            usageTime = [],
            rules = [],
            statistics = []
        }

    fromMySQLValues res = do
        vals <- res
        return (ResourcesInfo {
            tableName   = tableName emptyResourceStruct,
            fieldNames  = fieldNames emptyResourceStruct,
            ids         = map myToInt32 (genStruct vals 0),
            names       = map myToString (genStruct vals 1),
            types       = map myToString (genStruct vals 2),
            annotations = map myToString (genStruct vals 3),
            links       = map myToString (genStruct vals 4),
            purposes    = map myToString (genStruct vals 5),
            openDate    = map myToDay (genStruct vals 6),
            usageTime   = map myToInt32 (genStruct vals 7),
            rules       = map myToString (genStruct vals 8),
            statistics  = map myToInt32 (genStruct vals 9)
        })

    isEmpty tableInfo = null (ids tableInfo) || null (names tableInfo) || null (types tableInfo) || 
                        null (annotations tableInfo) || null (links tableInfo) || null (purposes tableInfo) || 
                        null (openDate tableInfo) || null (usageTime tableInfo) || null (rules tableInfo) || 
                        null (statistics tableInfo)

    len tableInfo = fromEnum (not (null (ids tableInfo))) +
                    fromEnum (not (null (names tableInfo))) +
                    fromEnum (not (null (types tableInfo))) +
                    fromEnum (not (null (annotations tableInfo))) +
                    fromEnum (not (null (links tableInfo))) +
                    fromEnum (not (null (purposes tableInfo))) +
                    fromEnum (not (null (openDate tableInfo))) +
                    fromEnum (not (null (usageTime tableInfo))) +
                    fromEnum (not (null (rules tableInfo))) +
                    fromEnum (not (null (statistics tableInfo)))

    printInfo tableInfo _ = putStrLn (TPrettyP.render draw ++ "\n")
        where
            draw :: TPrettyP.Doc
            draw = TPrettyP.text header
                TPrettyP.$+$
                    TPrettyP.text (genMyLine (length header +
                                              4 * length (filter (== '\t') header)))
                TPrettyP.$+$
                    TPrettyP.vcat (map row (mergeAll tableInfo))
                where
                    row t = TPrettyP.text( genRow (fieldNames tableInfo) t id)
                    header = genRow (fieldNames tableInfo) (fieldNames tableInfo) id
