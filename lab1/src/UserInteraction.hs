{-# LANGUAGE OverloadedStrings #-}
module UserInteraction where

import Database.MySQL.Base ( MySQLConn, OK (okAffectedRows), MySQLValue, putTextField )
import qualified Data.Text as T
import qualified Converter as Conv ( readInt, readStr, strToDay, strToInt32, strToDayUserInteract, myToInt32 )
import qualified MySQLConnector as MSQLC ( showTables, Table (addValue, getAllValues, printInfo, deleteValue, updateKeyField, updateField, getValue, drop) )
import qualified UsersTable as UT ( UsersInfo( UsersInfo, tableName, fieldNames, ids, emails, passwords, names ), emptyUserStruct )
import qualified AuthorsTable as AT ( AuthorsInfo( AuthorsInfo, tableName, fieldNames, ids, emails, names, surnames ), emptyAuthorStruct )
import qualified ResourcesTable as RT ( ResourcesInfo( ResourcesInfo, tableName, fieldNames, ids, names, types, annotations, links, purposes, openDate, usageTime, rules, statistics ), emptyResourceStruct )
import qualified AuthorsToResources as A2RT ( AuthorToResources ( AuthorToResources, tableName, fieldNames, ids, authorIds, resourceIds ), emptyAuthorToResourceStruct, getAuthorId, getResourceId )
import qualified UsersToResources as U2RT ( UserToResources ( UserToResources, tableName, fieldNames, ids, userIds, resourceIds, usageStart ), emptyUserToResourceStruct, getUserId )

mainMenu :: String
mainMenu = "main"

subMenu :: String
subMenu = "sub_menu"

subMenuForConnectors :: String
subMenuForConnectors = "sub_menu_for_connectors"

userUpdateM :: String
userUpdateM = "user_update_menu"

authorUpdateM :: String
authorUpdateM = "author_update_menu"

authorShowM :: String
authorShowM = "author_show_menu"

resourceUpdateM :: String
resourceUpdateM = "resource_update_menu"

resourceShowM :: String
resourceShowM = "resource_show_menu"

author2ReShowM :: String 
author2ReShowM = "author_2_resource_show_menu"

user2ReShowM :: String 
user2ReShowM = "user_2_resource_show_menu"

menu :: String -> String -> String
menu menuType tableName
    | menuType == mainMenu   = "Input with which table you want to work:\
                            \\n\t1 - users,\
                            \\n\t2 - authors,\
                            \\n\t3 - resources,\
                            \\n\t4 - author to resource,\
                            \\n\t5 - user to resource,\
                            \\n\t-1 - close db."
    | menuType == subMenu    = "Input what you want to do with table \"" ++ tableName ++ "\":\
                            \\n\t1 - add new,\
                            \\n\t2 - remove from table,\
                            \\n\t3 - update value,\
                            \\n\t4 - show all,\
                            \\n\t5 - show by value,\
                            \\n\t-1 - return to main menu."
    | menuType == subMenuForConnectors = "Input what you want to do with table \"" ++ tableName ++ "\":\
                            \\n\t1 - add new,\
                            \\n\t2 - remove from table,\
                            \\n\t3 - show all,\
                            \\n\t4 - show by value,\
                            \\n\t-1 - return to main menu."
    | menuType == userUpdateM = "Input what you want update in table \"users\":\
                            \\n\t1 - email for 1 user,\
                            \\n\t2 - username for 1 user,\
                            \\n\t3 - password hash for 1 user,\
                            \\n\t-1 - return to table menu."
    | menuType == authorUpdateM = "Input what you want update in table \"authors\":\
                            \\n\t1 - email for 1 author,\
                            \\n\t2 - surname for 1 author,\
                            \\n\t-1 - return to table menu."
    | menuType == authorShowM = "Input how you want to search in table \"authors\":\
                            \\n\t1 - by email,\
                            \\n\t2 - by surname,\
                            \\n\t-1 - return to table menu."
    | menuType == resourceUpdateM = "Input what you want update in table \"resources\":\
                            \\n\t1 - name title for 1 resource,\
                            \\n\t2 - link for 1 resource,\
                            \\n\t3 - change statisctics for 1 resource,\
                            \\n\t-1 - return to table menu."
    | menuType == resourceShowM = "Input how you want to search in table \"resources\":\
                            \\n\t1 - by name,\
                            \\n\t2 - by type,\
                            \\n\t3 - by link,\
                            \\n\t-1 - return to table menu."
    | menuType == author2ReShowM = "Input how you want to search in table \"author_owns\":\
                            \\n\t1 - by author email,\
                            \\n\t2 - by resource,\
                            \\n\t-1 - return to table menu."
    | menuType == user2ReShowM = "Input how you want to search in table \"user_owns\":\
                            \\n\t1 - by user email,\
                            \\n\t2 - by resource,\
                            \\n\t-1 - return to table menu."


intToTableName :: Int -> String
intToTableName value
    | value == 1 = UT.tableName UT.emptyUserStruct
    | value == 2 = AT.tableName AT.emptyAuthorStruct
    | value == 3 = RT.tableName RT.emptyResourceStruct
    | value == 4 = A2RT.tableName A2RT.emptyAuthorToResourceStruct
    | value == 5 = U2RT.tableName U2RT.emptyUserToResourceStruct
    | otherwise  = "table not exists"


intToFunc :: String -> Int -> (MySQLConn -> IO ())
intToFunc tableName value
    | value == 1 && tableName == UT.tableName UT.emptyUserStruct = addUser
    | value == 2 && tableName == UT.tableName UT.emptyUserStruct = deleteUser
    | value == 3 && tableName == UT.tableName UT.emptyUserStruct = updateUser
    | value == 4 && tableName == UT.tableName UT.emptyUserStruct = showAllUsers
    | value == 5 && tableName == UT.tableName UT.emptyUserStruct = showUserByEmail

    | value == 1 && tableName == AT.tableName AT.emptyAuthorStruct = addAuthor
    | value == 2 && tableName == AT.tableName AT.emptyAuthorStruct = deleteAuthor
    | value == 3 && tableName == AT.tableName AT.emptyAuthorStruct = updateAuthor
    | value == 4 && tableName == AT.tableName AT.emptyAuthorStruct = showAllAuthors
    | value == 5 && tableName == AT.tableName AT.emptyAuthorStruct = showAuthorBy

    | value == 1 && tableName == RT.tableName RT.emptyResourceStruct = addResource
    | value == 2 && tableName == RT.tableName RT.emptyResourceStruct = deleteResource
    | value == 3 && tableName == RT.tableName RT.emptyResourceStruct = updateResource
    | value == 4 && tableName == RT.tableName RT.emptyResourceStruct = showAllResources
    | value == 5 && tableName == RT.tableName RT.emptyResourceStruct = showResourceBy

    | value == 1 && tableName == A2RT.tableName A2RT.emptyAuthorToResourceStruct = addAuthor2Resource
    | value == 2 && tableName == A2RT.tableName A2RT.emptyAuthorToResourceStruct = deleteAuthor2Resource
    | value == 3 && tableName == A2RT.tableName A2RT.emptyAuthorToResourceStruct = showAllAuthor2Resource
    | value == 4 && tableName == A2RT.tableName A2RT.emptyAuthorToResourceStruct = showAuthor2Res

    | value == 1 && tableName == U2RT.tableName U2RT.emptyUserToResourceStruct = addUser2Resource
    | value == 2 && tableName == U2RT.tableName U2RT.emptyUserToResourceStruct = deleteUser2Resource
    | value == 3 && tableName == U2RT.tableName U2RT.emptyUserToResourceStruct = showAllUser2Resource
    | value == 4 && tableName == U2RT.tableName U2RT.emptyUserToResourceStruct = showUser2Res
    | otherwise                          = stub

addUser :: MySQLConn -> IO ()
addUser conn = do
    email <- Conv.readStr "Input email:\n"
    username <- Conv.readStr "Input username:\n"
    passwd_hash <- Conv.readStr "Input password hash:\n"
    let table = UT.UsersInfo {
            UT.tableName = UT.tableName UT.emptyUserStruct, UT.fieldNames = UT.fieldNames UT.emptyUserStruct,
            UT.ids = [], UT.emails = [email], UT.passwords = [passwd_hash], UT.names = [username]
        }
    response <- MSQLC.addValue table conn
    if okAffectedRows response == (-200)
        then putStrLn "### Email is in use (try again next time with another)\n"
        else putStrLn "### Added to table successfully\n"


deleteUser :: MySQLConn -> IO ()
deleteUser conn = do
    email <- Conv.readStr "Input email of user to delete:\n"
    let table = UT.UsersInfo {
            UT.tableName = UT.tableName UT.emptyUserStruct, UT.fieldNames = UT.fieldNames UT.emptyUserStruct,
            UT.ids = [], UT.emails = [email], UT.passwords = [], UT.names = []
        }
    response <- MSQLC.deleteValue table conn
    if okAffectedRows response == 0
        then putStrLn "### Email doesn't exist (try again next time with another)\n"
        else putStrLn "### Deleted from table successfully\n"

updateUserInteraction :: MySQLConn -> String -> Int -> IO ()
updateUserInteraction conn instanceName value = do
    email <- Conv.readStr "Input email of user to update:\n"
    let tableCompare = UT.UsersInfo {
                UT.tableName = UT.tableName UT.emptyUserStruct, UT.fieldNames = UT.fieldNames UT.emptyUserStruct,
                UT.ids = [], UT.emails = [email], UT.passwords = [], UT.names = []
            }
    valueToUpdate <- Conv.readStr "Input new value:\n"
    let tableUpdate
            | value == 1 = UT.UsersInfo {
                UT.tableName = UT.tableName UT.emptyUserStruct, UT.fieldNames = UT.fieldNames UT.emptyUserStruct,
                UT.ids = [], UT.emails = [valueToUpdate], UT.passwords = [], UT.names = []
            }
            | value == 2 = UT.UsersInfo {
                UT.tableName = UT.tableName UT.emptyUserStruct, UT.fieldNames = UT.fieldNames UT.emptyUserStruct,
                UT.ids = [], UT.emails = [], UT.passwords = [], UT.names = [valueToUpdate]
            }
            | otherwise = UT.UsersInfo {
                UT.tableName = UT.tableName UT.emptyUserStruct, UT.fieldNames = UT.fieldNames UT.emptyUserStruct,
                UT.ids = [], UT.emails = [], UT.passwords = [valueToUpdate], UT.names = []
            }
    res <- if value == 1
            then MSQLC.updateKeyField tableUpdate tableCompare conn
            else MSQLC.updateField tableUpdate tableCompare conn
    if okAffectedRows res == -100
        then putStrLn "### User not exists.\n"
        else if okAffectedRows res == -200
            then putStrLn "### New email is in use\n"
            else putStrLn "### Changed successfully!\n"

updateUser :: MySQLConn -> IO ()
updateUser conn =
    actionPrototype conn userUpdateM (UT.tableName UT.emptyUserStruct)
        "Exit to table \"users\"!\n" updateUserInteraction [1, 3]

showAllUsers :: MySQLConn -> IO ()
showAllUsers conn = do
    allValues <- MSQLC.getAllValues UT.emptyUserStruct conn
    MSQLC.printInfo allValues conn

showUserByEmail :: MySQLConn -> IO ()
showUserByEmail conn = do
    email <- Conv.readStr "Input email to search:\n"
    let tableInfo = UT.UsersInfo {
                UT.tableName = UT.tableName UT.emptyUserStruct, UT.fieldNames = UT.fieldNames UT.emptyUserStruct,
                UT.ids = [], UT.emails = [email], UT.passwords = [], UT.names = []
            }
    res <- MSQLC.getValue tableInfo conn
    MSQLC.printInfo res conn


addAuthor :: MySQLConn -> IO ()
addAuthor conn = do
    email <- Conv.readStr "Input email:\n"
    name <- Conv.readStr "Input name:\n"
    surname <- Conv.readStr "Input surname:\n"
    let table = AT.AuthorsInfo {
            AT.tableName = AT.tableName AT.emptyAuthorStruct,
            AT.fieldNames = AT.fieldNames AT.emptyAuthorStruct,
            AT.ids = [], AT.emails = [email], AT.names = [name], AT.surnames = [surname]
        }
    response <- MSQLC.addValue table conn
    if okAffectedRows response == -200
        then putStrLn "### Email is in use (try again next time with another)\n"
        else putStrLn "### Added to table successfully\n"


deleteAuthor :: MySQLConn -> IO ()
deleteAuthor conn = do
    email <- Conv.readStr "Input email of author to delete:\n"
    let table = AT.AuthorsInfo {
            AT.tableName = AT.tableName AT.emptyAuthorStruct,
            AT.fieldNames = AT.fieldNames AT.emptyAuthorStruct,
            AT.ids = [], AT.emails = [email], AT.names = [], AT.surnames = []
        }
    response <- MSQLC.deleteValue table conn
    if okAffectedRows response == 0
        then putStrLn "### Email doesn't exist (try again next time with another)\n"
        else putStrLn "### Deleted from table successfully\n"

updateAuthorInteraction :: MySQLConn -> String -> Int -> IO ()
updateAuthorInteraction conn instanceName value = do
    email <- Conv.readStr "Input email of author to update:\n"
    let tableCompare = AT.AuthorsInfo {
            AT.tableName = AT.tableName AT.emptyAuthorStruct,
            AT.fieldNames = AT.fieldNames AT.emptyAuthorStruct,
            AT.ids = [], AT.emails = [email], AT.names = [], AT.surnames = []
        }
    valueToUpdate <- Conv.readStr "Input new value:\n"
    let tableUpdate
            | value == 1 = AT.AuthorsInfo {
                AT.tableName = AT.tableName AT.emptyAuthorStruct,
                AT.fieldNames = AT.fieldNames AT.emptyAuthorStruct,
                AT.ids = [], AT.emails = [valueToUpdate], AT.names = [], AT.surnames = []
            }
            | value == 2 = AT.AuthorsInfo {
                AT.tableName = AT.tableName AT.emptyAuthorStruct,
                AT.fieldNames = AT.fieldNames AT.emptyAuthorStruct,
                AT.ids = [], AT.emails = [], AT.names = [], AT.surnames = [valueToUpdate]
            }
    res <- if value == 1
            then MSQLC.updateKeyField tableUpdate tableCompare conn
            else MSQLC.updateField tableUpdate tableCompare conn
    if okAffectedRows res == -100
        then putStrLn "### User not exists.\n"
        else if okAffectedRows res == -200
            then putStrLn "### New email is in use\n"
            else putStrLn "### Changed successfully!\n"

updateAuthor :: MySQLConn -> IO ()
updateAuthor conn =
    actionPrototype conn authorUpdateM (AT.tableName AT.emptyAuthorStruct)
        "Exit to table \"authors\"!\n" updateAuthorInteraction [1, 2]

showAllAuthors :: MySQLConn -> IO ()
showAllAuthors conn = do
    allValues <- MSQLC.getAllValues AT.emptyAuthorStruct conn
    MSQLC.printInfo allValues conn

showAuthorInteraction :: MySQLConn -> String -> Int -> IO ()
showAuthorInteraction conn instanceName value = do
    valueToSearch <- Conv.readStr "Input value to search:\n"
    let tableInfo
            | value == 1 = AT.AuthorsInfo {
                AT.tableName = AT.tableName AT.emptyAuthorStruct,
                AT.fieldNames = AT.fieldNames AT.emptyAuthorStruct,
                AT.ids = [], AT.emails = [valueToSearch], AT.names = [], AT.surnames = []
            }
            | value == 2 = AT.AuthorsInfo {
                AT.tableName = AT.tableName AT.emptyAuthorStruct,
                AT.fieldNames = AT.fieldNames AT.emptyAuthorStruct,
                AT.ids = [], AT.emails = [], AT.names = [], AT.surnames = [valueToSearch]
            }
    res <- MSQLC.getValue tableInfo conn
    MSQLC.printInfo res conn

showAuthorBy :: MySQLConn -> IO ()
showAuthorBy conn =
    actionPrototype conn authorShowM (AT.tableName AT.emptyAuthorStruct)
        "Exit to table \"authors\"!\n" showAuthorInteraction [1, 2]

addResource :: MySQLConn -> IO ()
addResource conn = do
    name <- Conv.readStr "Input name:\n"
    resourceType <- Conv.readStr "Input type (A, B or C):\n"
    annotations <- Conv.readStr "Input annotations:\n"
    link <- Conv.readStr "Input link:\n"
    purpose <- Conv.readStr "Input purpose:\n"
    openDate <- Conv.readStr "Input open date (%Y-%m-%d):\n"
    usageTime <- Conv.readStr "Input usage time (number of days):\n"
    rules <- Conv.readStr "Input rules:\n"
    statistics <- Conv.readStr "Input statistic:\n"
    let table = RT.ResourcesInfo {
            RT.tableName = RT.tableName RT.emptyResourceStruct,
            RT.fieldNames = RT.fieldNames RT.emptyResourceStruct,
            RT.ids = [], RT.names = [name], RT.types = [resourceType], RT.annotations = [annotations],
            RT.links = [link], RT.purposes = [purpose], RT.openDate = [Conv.strToDayUserInteract openDate],
            RT.usageTime = [Conv.strToInt32 usageTime], RT.rules = [rules],
            RT.statistics = [Conv.strToInt32 statistics]
        }
    response <- MSQLC.addValue table conn
    if okAffectedRows response == -200
        then putStrLn "### Article name is in use (try again next time with another)\n"
        else putStrLn "### Added to table successfully\n"


deleteResource :: MySQLConn -> IO ()
deleteResource conn = do
    name <- Conv.readStr "Input article name of resource to delete:\n"
    let table = RT.ResourcesInfo {
            RT.tableName = RT.tableName RT.emptyResourceStruct,
            RT.fieldNames = RT.fieldNames RT.emptyResourceStruct,
            RT.ids = [], RT.names = [name], RT.types = [], RT.annotations = [], RT.links = [],
            RT.purposes = [], RT.openDate = [], RT.usageTime = [], RT.rules = [], RT.statistics = []
        }
    response <- MSQLC.deleteValue table conn
    if okAffectedRows response == 0
        then putStrLn "### Article name doesn't exist (try again next time with another)\n"
        else putStrLn "### Deleted from table successfully\n"

updateResourceInteraction :: MySQLConn -> String -> Int -> IO ()
updateResourceInteraction conn instanceName value = do
    name <- Conv.readStr "Input name of resources to update:\n"
    let tableCompare = RT.ResourcesInfo {
            RT.tableName = RT.tableName RT.emptyResourceStruct,
            RT.fieldNames = RT.fieldNames RT.emptyResourceStruct,
            RT.ids = [], RT.names = [name], RT.types = [], RT.annotations = [], RT.links = [],
            RT.purposes = [], RT.openDate = [], RT.usageTime = [], RT.rules = [], RT.statistics = []
        }
    valueToUpdate <- Conv.readStr "Input new value:\n"
    let tableUpdate
            | value == 1 = RT.ResourcesInfo {
                RT.tableName = RT.tableName RT.emptyResourceStruct,
                RT.fieldNames = RT.fieldNames RT.emptyResourceStruct,
                RT.ids = [], RT.names = [valueToUpdate], RT.types = [], RT.annotations = [], RT.links = [],
                RT.purposes = [], RT.openDate = [], RT.usageTime = [], RT.rules = [], RT.statistics = []
            }
            | value == 2 = RT.ResourcesInfo {
                RT.tableName = RT.tableName RT.emptyResourceStruct,
                RT.fieldNames = RT.fieldNames RT.emptyResourceStruct,
                RT.ids = [], RT.names = [], RT.types = [], RT.annotations = [], RT.links = [valueToUpdate],
                RT.purposes = [], RT.openDate = [], RT.usageTime = [], RT.rules = [], RT.statistics = []
            }
            | value == 3 = RT.ResourcesInfo {
                RT.tableName = RT.tableName RT.emptyResourceStruct,
                RT.fieldNames = RT.fieldNames RT.emptyResourceStruct,
                RT.ids = [], RT.names = [], RT.types = [], RT.annotations = [], RT.links = [],
                RT.purposes = [], RT.openDate = [], RT.usageTime = [], RT.rules = [],
                RT.statistics = [Conv.strToInt32 valueToUpdate]
            }

    res <- if value == 1
            then MSQLC.updateKeyField tableUpdate tableCompare conn
            else MSQLC.updateField tableUpdate tableCompare conn
    if okAffectedRows res == -100
        then putStrLn "### Resource not exists.\n"
        else if okAffectedRows res == -200
            then putStrLn "### New resource name is in use\n"
            else putStrLn "### Changed successfully!\n"

updateResource :: MySQLConn -> IO ()
updateResource conn =
    actionPrototype conn resourceUpdateM (RT.tableName RT.emptyResourceStruct)
        "Exit to table \"resources\"!\n" updateResourceInteraction [1, 3]

showAllResources :: MySQLConn -> IO ()
showAllResources conn = do
    allValues <- MSQLC.getAllValues RT.emptyResourceStruct conn
    MSQLC.printInfo allValues conn

showResourceByInteraction :: MySQLConn -> String -> Int -> IO ()
showResourceByInteraction conn instanceName value = do
    valueToSearch <- Conv.readStr "Input value to search:\n"
    let tableInfo
            | value == 1 = RT.ResourcesInfo {
                RT.tableName = RT.tableName RT.emptyResourceStruct,
                RT.fieldNames = RT.fieldNames RT.emptyResourceStruct,
                RT.ids = [], RT.names = [valueToSearch], RT.types = [], RT.annotations = [], RT.links = [],
                RT.purposes = [], RT.openDate = [], RT.usageTime = [], RT.rules = [], RT.statistics = []
            }
            | value == 2 = RT.ResourcesInfo {
                RT.tableName = RT.tableName RT.emptyResourceStruct,
                RT.fieldNames = RT.fieldNames RT.emptyResourceStruct,
                RT.ids = [], RT.names = [], RT.types = [valueToSearch], RT.annotations = [], RT.links = [],
                RT.purposes = [], RT.openDate = [], RT.usageTime = [], RT.rules = [], RT.statistics = []
            }
            | value == 3 = RT.ResourcesInfo {
                RT.tableName = RT.tableName RT.emptyResourceStruct,
                RT.fieldNames = RT.fieldNames RT.emptyResourceStruct,
                RT.ids = [], RT.names = [], RT.types = [], RT.annotations = [], RT.links = [valueToSearch],
                RT.purposes = [], RT.openDate = [], RT.usageTime = [], RT.rules = [],
                RT.statistics = []
            }

    res <- MSQLC.getValue tableInfo conn
    MSQLC.printInfo res conn

showResourceBy :: MySQLConn -> IO ()
showResourceBy conn =
    actionPrototype conn resourceShowM (RT.tableName RT.emptyResourceStruct)
        "Exit to table \"resources\"!\n" showResourceByInteraction [1, 3]


addAuthor2Resource :: MySQLConn -> IO ()
addAuthor2Resource conn = do
    email <- Conv.readStr "Input author email:\n"
    name <- Conv.readStr "Input resource name:\n"
    authorId <- A2RT.getAuthorId email conn
    nameId <- A2RT.getResourceId name conn
    if authorId == (-1)
    then putStrLn "### Author doesn't exist\n"
    else if nameId == (-1)
    then putStrLn "### Resource doesn't exist\n"
    else do
        let table = A2RT.AuthorToResources {
                A2RT.tableName = A2RT.tableName A2RT.emptyAuthorToResourceStruct,
                A2RT.fieldNames = A2RT.fieldNames A2RT.emptyAuthorToResourceStruct,
                A2RT.ids = [], A2RT.authorIds = [authorId], A2RT.resourceIds = [nameId]
            }
        response <- MSQLC.addValue table conn
        if okAffectedRows response == -200
            then putStrLn "### Record already exists (try again next time with another)\n"
            else putStrLn "### Added to table successfully\n"


deleteAuthor2Resource :: MySQLConn -> IO ()
deleteAuthor2Resource conn = do
    email <- Conv.readStr "Input email of author to delete:\n"
    name <- Conv.readStr "Input name of resource to delete:\n"
    authorId <- A2RT.getAuthorId email conn
    nameId <- A2RT.getResourceId name conn
    if authorId == (-1)
    then putStrLn "### Author doesn't exist\n"
    else if nameId == (-1)
    then putStrLn "###  Resource doesn't exist\n"
    else do
        let table = A2RT.AuthorToResources {
                    A2RT.tableName = A2RT.tableName A2RT.emptyAuthorToResourceStruct,
                    A2RT.fieldNames = A2RT.fieldNames A2RT.emptyAuthorToResourceStruct,
                    A2RT.ids = [], A2RT.authorIds = [authorId], A2RT.resourceIds = [nameId]
                }
        response <- MSQLC.deleteValue table conn
        if okAffectedRows response == 0
            then putStrLn "### Record doesn't exist (try again next time with another)\n"
            else putStrLn "### Deleted from table successfully\n"

showAllAuthor2Resource :: MySQLConn -> IO ()
showAllAuthor2Resource conn = do
    allValues <- MSQLC.getAllValues A2RT.emptyAuthorToResourceStruct conn
    MSQLC.printInfo allValues conn

showAuthor2ResInteraction :: MySQLConn -> String -> Int -> IO ()
showAuthor2ResInteraction conn instanceName value = do
    valueToUpdate <- Conv.readStr "Input value to search:\n"
    res <- if value == 1 
            then A2RT.getAuthorId valueToUpdate conn
            else A2RT.getResourceId valueToUpdate conn
    let tableInfo
            | value == 1 = A2RT.AuthorToResources {
                    A2RT.tableName = A2RT.tableName A2RT.emptyAuthorToResourceStruct,
                    A2RT.fieldNames = A2RT.fieldNames A2RT.emptyAuthorToResourceStruct,
                    A2RT.ids = [], A2RT.authorIds = [res], A2RT.resourceIds = []
                }
            | value == 2 = A2RT.AuthorToResources {
                    A2RT.tableName = A2RT.tableName A2RT.emptyAuthorToResourceStruct,
                    A2RT.fieldNames = A2RT.fieldNames A2RT.emptyAuthorToResourceStruct,
                    A2RT.ids = [], A2RT.authorIds = [], A2RT.resourceIds = [res]
                }
    res <- MSQLC.getValue tableInfo conn
    MSQLC.printInfo res conn

showAuthor2Res :: MySQLConn -> IO ()
showAuthor2Res conn =
    actionPrototype conn author2ReShowM (A2RT.tableName A2RT.emptyAuthorToResourceStruct)
        "Exit to table \"author_owns\"!\n" showAuthor2ResInteraction [1, 2]


addUser2Resource :: MySQLConn -> IO ()
addUser2Resource conn = do
    email <- Conv.readStr "Input user email:\n"
    name <- Conv.readStr "Input resource name:\n"
    usageSt <- Conv.readStr "Input start day of usage (%Y-%m-d):\n"
    userId <- U2RT.getUserId email conn
    nameId <- A2RT.getResourceId name conn
    if userId == (-1)
    then putStrLn "### User doesn't exist\n"
    else if nameId == (-1)
    then putStrLn "### Resource doesn't exist\n"
    else do
        let table = U2RT.UserToResources {
                U2RT.tableName = U2RT.tableName U2RT.emptyUserToResourceStruct ,
                U2RT.fieldNames = U2RT.fieldNames U2RT.emptyUserToResourceStruct,
                U2RT.ids = [], U2RT.userIds = [userId], U2RT.resourceIds = [nameId],
                U2RT.usageStart = [Conv.strToDayUserInteract usageSt]
            }
        response <- MSQLC.addValue table conn
        if okAffectedRows response == -200
            then putStrLn "### Record already exists (try again next time with another)\n"
            else putStrLn "### Added to table successfully\n"


deleteUser2Resource :: MySQLConn -> IO ()
deleteUser2Resource conn = do
    email <- Conv.readStr "Input email of user to delete:\n"
    name <- Conv.readStr "Input name of resource to delete:\n"
    userId <- U2RT.getUserId email conn
    nameId <- A2RT.getResourceId name conn
    if userId == (-1)
    then putStrLn "### User doesn't exist\n"
    else if nameId == (-1)
    then putStrLn "### Resource doesn't exist\n"
    else do
        let table = U2RT.UserToResources {
                U2RT.tableName = U2RT.tableName U2RT.emptyUserToResourceStruct ,
                U2RT.fieldNames = U2RT.fieldNames U2RT.emptyUserToResourceStruct,
                U2RT.ids = [], U2RT.userIds = [userId], U2RT.resourceIds = [nameId],
                U2RT.usageStart = []
            }
        response <- MSQLC.deleteValue table conn
        if okAffectedRows response == 0
            then putStrLn "### Record doesn't exist (try again next time with another)\n"
            else putStrLn "### Deleted from table successfully\n"

showAllUser2Resource :: MySQLConn -> IO ()
showAllUser2Resource conn = do
    allValues <- MSQLC.getAllValues U2RT.emptyUserToResourceStruct conn
    MSQLC.printInfo allValues conn

showUser2ResInteraction :: MySQLConn -> String -> Int -> IO ()
showUser2ResInteraction conn instanceName value = do
    valueToSearch <- Conv.readStr "Input value to search:\n"
    res <- if value == 1 
            then U2RT.getUserId valueToSearch conn
            else A2RT.getResourceId valueToSearch conn
    let tableInfo
            | value == 1 = U2RT.UserToResources {
                    U2RT.tableName = U2RT.tableName U2RT.emptyUserToResourceStruct ,
                    U2RT.fieldNames = U2RT.fieldNames U2RT.emptyUserToResourceStruct,
                    U2RT.ids = [], U2RT.userIds = [res], U2RT.resourceIds = [],
                    U2RT.usageStart = []
                }       
            | value == 2 = U2RT.UserToResources {
                    U2RT.tableName = U2RT.tableName U2RT.emptyUserToResourceStruct ,
                    U2RT.fieldNames = U2RT.fieldNames U2RT.emptyUserToResourceStruct,
                    U2RT.ids = [], U2RT.userIds = [], U2RT.resourceIds = [res],
                    U2RT.usageStart = []
                }       
    res <- MSQLC.getValue tableInfo conn
    MSQLC.printInfo res conn

showUser2Res :: MySQLConn -> IO ()
showUser2Res conn =
    actionPrototype conn user2ReShowM (U2RT.tableName U2RT.emptyUserToResourceStruct)
        "Exit to table \"user_owns\"!\n" showUser2ResInteraction [1, 2]

stub :: MySQLConn -> IO ()
stub conn = print =<< MSQLC.showTables conn

dropAll :: MySQLConn -> IO ()
dropAll conn = do
    res <- MSQLC.drop U2RT.emptyUserToResourceStruct conn
    res <- MSQLC.drop A2RT.emptyAuthorToResourceStruct conn
    res <- MSQLC.drop RT.emptyResourceStruct conn
    res <- MSQLC.drop AT.emptyAuthorStruct conn
    res <- MSQLC.drop UT.emptyUserStruct conn
    putStrLn "Dropped all"

tableInteraction :: MySQLConn -> String -> Int -> IO ()
tableInteraction conn instanceName value = intToFunc instanceName value conn

subTableAction :: MySQLConn -> String -> Int -> IO ()
subTableAction conn instanceName value = do
    putStrLn ("----- Work with \"" ++ intToTableName value ++ "\" table -----")
    if value < 4
        then actionPrototype conn subMenu (intToTableName value) "Exit to main!\n" tableInteraction [1, 5]
        else actionPrototype conn subMenuForConnectors (intToTableName value) "Exit to main!\n" tableInteraction [1, 4]

action :: MySQLConn -> String -> Maybe Int -> (MySQLConn -> String -> Int -> IO ()) -> [Int] -> IO ()
action conn instanceName value func (x:y:xs) =
    case value of
        Just n -> if (n >= x) && (n <= y)
                    then func conn instanceName n
                    else putStrLn "Wrong input\n"
        Nothing -> putStrLn "Wrong input\n"

actionPrototype :: MySQLConn -> String -> String -> String -> (MySQLConn -> String -> Int -> IO ()) -> [Int]-> IO ()
actionPrototype conn menuType instanceName endString func range = do
    putStrLn (menu menuType instanceName)

    curAction <- Conv.readInt ""
    if curAction == Just (-1)
        then putStrLn endString
        else do
            action conn instanceName curAction func range
            actionPrototype conn menuType instanceName endString func range

doAction :: MySQLConn -> IO ()
doAction conn = actionPrototype conn mainMenu mainMenu "Goodbye!" subTableAction [1, 5]


