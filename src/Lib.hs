{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
module Lib where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Catch (Exception, throwM)
import           Data.Hashable
import           Haxl.Core

type Haxl a = GenHaxl () () a

data UserRequest a where
  FetchUser :: Int -> UserRequest User

deriving instance Eq (UserRequest a)
deriving instance Show (UserRequest a)

instance Hashable (UserRequest a) where
  hashWithSalt salt (FetchUser x) = hashWithSalt salt (0 :: Int, x)

instance ShowP UserRequest where
  showp (FetchUser x) = "FetchUser:" <> show x

instance StateKey UserRequest where
  data State UserRequest = UserDataState DBConnection

instance DataSourceName UserRequest where
  dataSourceName _ = "UserRequest"

instance DataSource u UserRequest where
  fetch (UserDataState conn) f env = SyncFetch $ \xs -> do
    print ("fetch user", length xs)
    batchFetch conn xs

initDataSource :: IO (State UserRequest)
initDataSource = pure $ UserDataState ()


getUser :: Int -> GenHaxl u w User
getUser = dataFetch . FetchUser


batchFetch :: DBConnection -> [BlockedFetch UserRequest] -> IO ()
batchFetch conn xs = do
  us <- queryUsersByIds conn ids
  let
    lookup' :: (BlockedFetch UserRequest -> IO ())
    lookup' (BlockedFetch (FetchUser x) rv) =
        case lookup x us of
          Just u  -> putSuccess rv u
          Nothing -> putFailure rv ResourceNotExist
  mapM_ lookup' xs
  where
    ids = map (\(BlockedFetch (FetchUser x) _) -> x) xs
    -- f :: (BlockedFetch UserRequest -> IO ())
    -- f (BlockedFetch (FetchUser _) rv) = putSuccess rv []


data ServiceException =
  ResourceNotExist
  | OtherException String
  deriving (Show, Eq)
instance Exception ServiceException

----

type DBConnection = ()

data User = User
  { id   :: Int
  , name :: String
  } deriving (Show)

users :: [User]
users =
  [ User 123 "daishi nakajima"
  , User 124 "hoge"
  , User 456 "fuga"
  ]

queryUserById :: DBConnection -> Int -> IO User
queryUserById _ _ =
  -- select * from user where id = ?
  pure $ head users

queryUsersByIds :: DBConnection -> [Int] -> IO [(Int, User)]
queryUsersByIds _ _ =
  pure $ [ (Lib.id x, x) | x <- users]

----

run :: IO [User]
run = do
  db <- initDataSource
  let s = stateSet db stateEmpty
  e <- initEnv s ()
  runHaxl e $
    mapM getUser [123, 124, 456]
