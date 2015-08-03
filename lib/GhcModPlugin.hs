{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GhcModPlugin
    ( emptyState
    , testGhcMod
    , GhcState
    ) where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.MVar
import           Control.Exception (bracket)
import           Control.Monad (forever)
import           Control.Monad.Error (MonadError(..))
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as B
import           Data.Map (Map)
import qualified Data.Map as Map
import           System.Directory (setCurrentDirectory, getCurrentDirectory)
import           System.FilePath (takeDirectory)
import           System.IO (stderr, hPutStrLn)

import           Language.Haskell.GhcMod
import           Language.Haskell.GhcMod.Internal
import           Neovim
import           Neovim.API.String

------------------------------------------------------------------------

newtype GhcRunner = GhcRunner {
      runRunner :: forall a. GhcModT IO a -> GhcPlugin a
    }

type RootDir   = FilePath
type HaskellFile = FilePath

data GhcState = GhcState {
      ghcRunners :: Map RootDir GhcRunner
    , ghcFiles   :: Map HaskellFile RootDir
    }

type GhcPlugin a = Neovim () GhcState a

------------------------------------------------------------------------
-- Plugin initialization

emptyState :: GhcState
emptyState = GhcState Map.empty Map.empty


------------------------------------------------------------------------
-- ghc-mod thread/runner creation
--   This is to workaround the fact that ghc-mod doesn't expose enough of the
--   internals to be able to reuse its state between calls to `runGhcModT`.

getRunner :: HaskellFile -> GhcPlugin GhcRunner
getRunner file = do
    root    <- getRootDir file
    mrunner <- gets (Map.lookup root . ghcRunners)
    case mrunner of
      Just runner -> return runner
      Nothing     -> do
        runner <- liftIO (forkRunner root)
        modify (\s -> s { ghcRunners = Map.insert root runner (ghcRunners s) })
        return runner

forkRunner :: RootDir -> IO GhcRunner
forkRunner dir = do
    (inp :: MVar (GhcModT IO ()))  <- newEmptyMVar
    old <- getCurrentDirectory

    _ <- forkIO $ do
        setCurrentDirectory dir

        _ <- runGhcModT defaultOptions $ do
          liftIO (setCurrentDirectory old)

          -- TODO Error Handling
          forever $ do
            action <- liftIO (takeMVar inp)
            old'   <- liftIO getCurrentDirectory
            liftIO (setCurrentDirectory dir)
            action
            liftIO (setCurrentDirectory old')

        return ()

    let runner request = liftIO $ do
          out <- newEmptyMVar
          putMVar inp $ do
            response <- request
            liftIO (putMVar out response)
          takeMVar out

    return (GhcRunner runner)

------------------------------------------------------------------------
-- Project root directory discovery

getRootDir :: HaskellFile -> GhcPlugin RootDir
getRootDir file = do
    mroot <- gets (Map.lookup file . ghcFiles)
    case mroot of
      Just root -> return root
      Nothing   -> do
        root <- liftIO (findRootDir file)
        modify (\s -> s { ghcFiles = Map.insert file root (ghcFiles s) })
        return root

findRootDir :: HaskellFile -> IO RootDir
findRootDir file =
    bracket getCurrentDirectory setCurrentDirectory (const find)
  where
    dir  = takeDirectory file
    find = setCurrentDirectory dir >> findCradle >>= return . cradleRootDir


------------------------------------------------------------------------

testGhcMod :: GhcPlugin String
testGhcMod = do
    Right (ObjectBinary bspath) <- wait (vim_call_function "expand" [ObjectString "%:p"])
    let path = B.unpack bspath

    --s <- get
    --runner <- getRunner path

    --c <- runRunner runner (info path (Expression "concat"))
    --c <- runRunner runner (browse "Data.List")
    --c <- runRunner runner debugInfo

    c <- liftIO (runGhcMod path defaultOptions (info path (Expression "concat")))

    return (show c)

    -- return $ unlines [
    --     show c
    --   , show (Map.keys (ghcRunners s))
    --   , show (ghcFiles s)
    --   ]


------------------------------------------------------------------------

runGhcMod :: FilePath -> Options -> GhcModT IO a -> IO (Either GhcModError a, GhcModLog)
runGhcMod path options ghcmod =
    bracket getCurrentDirectory setCurrentDirectory (const run)
  where
    dir = takeDirectory path
    run = setCurrentDirectory dir >> runGhcModT options ghcmod
