{-# LANGUAGE DeriveGeneric #-}

module Data.Todo
where 

import Data.Aeson
import GHC.Generics

type Name = String
data Priority = High | Medium | Low deriving (Generic, Show)

data TodoList = TodoList {projects :: [Project]} deriving (Generic, Show)
data Project  = Project {projectName :: Name, repos :: [Repo]} deriving (Generic, Show)
data Repo     = Repo {repoName :: Name, tasks :: [RepoTask]} deriving (Generic, Show)
data RepoTask = RepoTask {description :: Name, priority :: Priority} deriving (Generic, Show)

instance ToJSON TodoList
instance ToJSON Project
instance ToJSON Repo
instance ToJSON RepoTask
instance ToJSON Priority

instance FromJSON TodoList
instance FromJSON Project
instance FromJSON Repo
instance FromJSON RepoTask
instance FromJSON Priority


list = TodoList
    {projects = 
        [ Project 
          { projectName="yield-calibration"
          , repos=[ Repo{ repoName="app-api"
                        , tasks=[RepoTask {description="Update Topics in Env"
                                          , priority=Medium
                                          }
                                
                                , RepoTask{description="Add Feature Flag check to endpoint validation"
                                          , priority=Medium
                                          }
                              ]
                        }
                  ]
          }
        ]
    }
