{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Todo where

import GHC.Generics ( Generic )
import Data.Aeson (FromJSON, ToJSON)
import Data.List (find)
import Data.Maybe ( fromMaybe )
import Data.Named ( Named(..), Name, (<..>) )

data Priority           = Low | Medium | High | Blocker  deriving (Generic, Show, Eq, Ord)     
type Component          = String  
data RepoTask           = RepoTask {taskName :: Name, priority :: Priority, components :: [Component]} deriving (Generic, Show)
data Repo               = Repo {repoName :: Name, tasks :: [RepoTask]} deriving (Generic, Show)
data Project            = Project {projectName :: Name, repos :: [Repo]} deriving (Generic, Show)
newtype TodoList        = TodoList {projects :: [Project]} deriving (Generic, Show)


instance ToJSON TodoList
instance FromJSON TodoList
instance Semigroup TodoList where
    (<>) (TodoList a) (TodoList a') = 
        TodoList (a <..> a')
instance Monoid TodoList where
    mempty = TodoList []

instance ToJSON Project
instance FromJSON Project
instance Semigroup Project where
    (<>) (Project n rs) (Project n' rs') 
        | n == n'                       = Project {projectName=n, repos= rs <..> rs'}
        | n == "" || n' == ""           = Project {projectName=n ++ n', repos=rs <..> rs'}
        | otherwise                     = Project {projectName=n ++ ", " ++ n', repos=rs <..> rs'}
instance Monoid Project where
    mempty = Project 
        { projectName=""
        , repos=[]
        }
instance Named Project where
    getName = projectName

instance ToJSON Repo
instance FromJSON Repo
instance Semigroup Repo where
    (<>) (Repo n ts) (Repo n' ts') 
        | n == n'                       = Repo n (ts <..> ts') 
        | n == "" || n' == ""           = Repo (n ++ n') (ts <..> ts')
        | otherwise                     = Repo (n ++ ", " ++ n') (ts <..> ts')
instance Monoid Repo where
    mempty = Repo 
        { repoName=""
        , tasks=[]
        }
instance Named Repo where
    getName = repoName

instance ToJSON RepoTask
instance FromJSON RepoTask
instance Semigroup RepoTask where
    (<>) repoTaskA repoTaskB = RepoTask 
                        { taskName=taskName repoTaskA
                        , priority=max (priority repoTaskA) (priority repoTaskB)
                        , components=components repoTaskA ++ components repoTaskB
                        }
instance Monoid RepoTask where
    mempty = RepoTask 
        { taskName=""
        , priority=Low
        , components=[]
        }
instance Named RepoTask where
    getName = taskName

instance ToJSON Priority
instance FromJSON Priority
