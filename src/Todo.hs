{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Todo where

import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)
import Data.List (find)
import Data.Maybe


class Named a where
    getName :: a -> Name


data Priority           = Low | Medium | High | Blocker  deriving (Generic, Show, Eq, Ord)     
type Name               = String
type Component          = String  
data RepoTask           = RepoTask {taskName :: Name, priority :: Priority, components :: [Component]} deriving (Generic, Show)
data Repo               = Repo {repoName :: Name, tasks :: [RepoTask]} deriving (Generic)
data Project            = Project {projectName :: Name, repos :: [Repo]} deriving (Generic)
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
instance Show Project where
    show p = "Project: " ++ projectName p ++ "\nr" ++ "\t" ++ fmap show repos p

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
instance Show Repo where
    show r = "Repo Name: " ++ repoName r ++ "\n\r" ++ fmap show tasks r
instance Semigroup Repo where
    (<>) (Repo n ts) (Repo n' ts') 
        | n == n'                       = Repo n (ts <..> ts') 
        | n == "" || n' == ""           = Repo (n ++ n') (ts <..> ts')
        | otherwise                     = Repo (n ++ ", " ++ n') (ts <..> ts')
instance Monoid Repo where
    mempty = Repo {repoName="", tasks=[]}
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
    mempty = RepoTask {taskName="", priority=Low, components=[]}

instance Named RepoTask where
    getName = taskName

instance ToJSON Priority
instance FromJSON Priority

matchesN :: Named a => a -> a -> Bool
matchesN a a' = getName a == getName a'

hasN :: Named a => a -> [a] -> Bool
hasN n ns = getName n `elem` fmap getName ns

filterN :: Named a => a -> [a] -> [a]
filterN n = filter $ not . matchesN n

findN :: (Named a, Monoid a) => a -> [a] -> a
findN n ns = fromMaybe mempty match
             where match = find (matchesN n) ns

infix 6 <.> 
(<.>) :: (Named a, Monoid a) => [a] -> a -> [a]
(<.>) ns n = if not $ hasN n ns then ns ++ [n] else n <> findN n ns : filterN n ns

infix 6 <..>
(<..>) :: (Named a, Monoid a) => [a] -> [a] -> [a]
(<..>) [] [] = []
(<..>) [] ns' = ns'
(<..>) ns [] = ns
(<..>) ns (n: ns') 
      | hasN n ns =  (ns <.> n) <..> ns'
      | otherwise  = (n : ns) <..> ns'
