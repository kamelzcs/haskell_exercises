module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

import Control.Concurrent.MVar

-- The task is to create the data type `BankAccount` and
-- and implement the functions below.

data BankAccount = BankAccount {getInfo:: MVar (Maybe Integer)}

closeAccount :: BankAccount -> IO ()
closeAccount acc = modifyMVar_ (getInfo acc) (const $ return Nothing)

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance = readMVar . getInfo

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance acc v = do
  modifyMVar_ (getInfo acc) (return . fmap (+v))
  getBalance acc

openAccount :: IO BankAccount
openAccount = fmap BankAccount $ newMVar $ Just 0
