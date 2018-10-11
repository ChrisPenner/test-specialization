{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Lib where

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable

newtype IOT m a =
  IOT (IO a)

type family GetStack m :: ((* -> *) -> * -> *) where
  GetStack IO = IOT
  GetStack (State s) = StateT s

newtype AppM filesystem logging m a =
  AppM (m a)
  deriving (Functor, Applicative, Monad)

unApp :: AppM a b m a -> m a
unApp (AppM m) = m

data Logging
  = LogIO
  | LogState

data FileSystem
  = FilesIO
  | FileReader

class (Monad m) =>
      MonadFiles (t :: FileSystem) m
  | m -> t
  where
  readAFile :: FilePath -> m String

class (Monad m) =>
      MonadLog (t :: Logging) m
  | m -> t
  where
  logMessage :: String -> m ()

instance (MonadIO m, Monad m) =>
         MonadFiles FilesIO (AppM FilesIO logging m) where
  readAFile f = AppM (liftIO $ readFile f)

instance (MonadIO m) => MonadLog LogIO (AppM filesystem LogIO m) where
  logMessage = AppM . liftIO . print

instance (MonadState [String] m) =>
         MonadLog LogState (AppM filesystem LogState m) where
  logMessage message = AppM $ modify (message :)

instance (MonadReader String m) =>
         MonadFiles FileReader (AppM FileReader logging m) where
  readAFile _ = AppM ask

prog :: (MonadLog l m, MonadFiles f m) => m Bool
prog = do
  contents <- readAFile "data.txt"
  logMessage contents
  return True

myTests :: [IO Bool]
myTests = [testRunLog . testRunFiles . unApp $ prog]

-- runIO :: AppM fs lg IO a -> IO a
-- runIO (AppM)
test1 :: (Monad m) => AppM fs lg m Bool
test1 = return True

testRunFiles :: ReaderT String m a -> m a
testRunFiles = flip runReaderT "testfile"

testRunLog :: Monad m => StateT [String] m a -> m a
testRunLog = flip evalStateT []

specialize :: AppM fs lg m a -> m a
specialize (AppM m) = m

runTests :: IO ()
runTests = traverse_ runTest myTests

runTest :: IO Bool -> IO ()
runTest io = do
  result <- io
  if result
    then print "1 Success"
    else print "1 Failure"
