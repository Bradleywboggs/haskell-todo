{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment ( getArgs, getEnv ) 
import Data.Char (toLower)
import System.IO 

import Data.Aeson
import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)

import Data.List (find)
import Data.Maybe
import GHC.Base (when)
import Todo

main :: IO ()
main = do
    (cmd: _) <- getArgs
    case cmd of
        "add" -> createTasks
        "show" -> display
        "clear" -> clearTodoList
        _     -> putStrLn "nope" 

    -- NEXT TASK: Add json decode/encode to the TodoItem type
    -- todo app
    -- read args to determine mode
    -- ADD
    -- read, add, edit, check-off, cleanup (dates-- defaults to all prior), 
        -- determine todays date on launch
        -- if we already have a task list, open the file
        -- take a project, description and a title and a urgency level (low priority, normal, high priority)
        -- onSave
        --  iterates through the existing file and inserts the new task under the project heading and places it by priorty


clearTodoList :: IO ()
clearTodoList = do
    file <- getEnv "TODO_FILE"
    writeFile file ""
    
display :: IO ()
display = do 
    file <- getEnv "TODO_FILE"
    contents <- decodeFileStrict file
    case contents of
        Nothing -> putStrLn "Todo file is empty. Please add a task."
        Just todoList -> mapM_ print (projects todoList)
    


getPriority :: IO Priority
getPriority = do
    selection <- mapM_ putStrLn ["\tWhat priority?","", "\t[1] Blocker\t[2] High", "\t[3] Medium", "\t[4] Low"] >> getLine
    case selection of 
        "1" -> return Blocker
        "2" -> return High
        "3" -> return Medium
        "4" -> return Low
        _   -> putStrLn "Please enter 1, 2, 3, or 4" >> getPriority

addMoreComponents :: IO Bool
addMoreComponents = do
    maybeResponse <- putStrLn "Do you want to add another component (y/n)" >> getLine
    case maybeResponse of
        "y" -> return True
        "n" -> return False
        _   -> putStrLn "That is an invalid entry. Enter a y or n" >> addMoreComponents
 

getComponents :: [Component] -> IO [Component]
getComponents list = do
    comp      <- putStrLn "Describe a component of the task" >> getLine
    shouldAdd <- addMoreComponents
    if shouldAdd 
        then getComponents (list <> [comp]) 
        else return (list <> [comp])

addMoreTasks :: IO Bool 
addMoreTasks = do
    resp <- putStrLn "Do you want to add another task? (Y/n)" >> getLine
    case resp of
        ""  -> return True 
        "y" -> return True 
        "Y" -> return True
        "n" -> return False 
        "N" -> return False
        _   -> putStrLn "Please select Y or N" >> addMoreTasks

createTasks :: IO ()
createTasks = do
    addTask
    shouldAdd <- addMoreTasks
    when shouldAdd addTask

addTask :: IO ()
addTask = do
    file         <- getEnv   "TODO_FILE"
    project     <- putStrLn "What project?"     >> getLine 
    repo        <- putStrLn "What repo?"        >> getLine 
    description <- putStrLn "Describe the task" >> getLine
    priority    <- getPriority
    components  <- getComponents mempty
    let newList = TodoList 
            { projects=
                [ Project
                   { projectName=project
                   , repos=
                       [ Repo
                        { repoName=repo
                        , tasks=
                            [ RepoTask
                                { taskName=description
                                , priority=priority
                                , components=components
                                }
                            ]
                        }
                       ]
                    }
                ]
            }
    maybeOriginal <- decodeFileStrict file :: IO (Maybe TodoList)
    case maybeOriginal of 
        Nothing -> encodeFile file newList
        Just original -> encodeFile file (original <> newList)
    

