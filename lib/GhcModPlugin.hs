{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module GhcModPlugin 
    ( testGhcMod
    , GhcModEnv
    , GhcModState
    ) where

import qualified Data.ByteString.Char8 as B
import           Control.Exception (bracket)
import           Control.Monad.IO.Class (liftIO)
import           System.Directory (setCurrentDirectory, getCurrentDirectory)
import           System.FilePath (takeDirectory)

import           Language.Haskell.GhcMod
import           Language.Haskell.GhcMod.Internal
import           Neovim
import           Neovim.API.String

------------------------------------------------------------------------

type GhcModPlugin a = Neovim () (Maybe (GhcModEnv, GhcModState)) a

------------------------------------------------------------------------

testGhcMod :: GhcModPlugin String
testGhcMod = do
    Right (ObjectBinary bspath) <- wait (vim_call_function "expand" [ObjectString "%:p"])
    let path = B.unpack bspath
    c <- liftIO (runGhcMod path defaultOptions (info path (Expression "concat")))
    return (show c)

------------------------------------------------------------------------

--initState :: FilePath -> GhcModPlugin (GhcModEnv, GhcModState)
--initState path = do
--    s <- get
--    case s of
--      Just _  -> s
--      Nothing -> do
--        let state = defaultGhcModState
--        let wdir  = takeDirectory path
--        let env   = GhcModEnv wdir defaultOptions
--        put (Just (env, state))
--        get

------------------------------------------------------------------------

runGhcMod :: FilePath -> Options -> GhcModT IO a -> IO (Either GhcModError a, GhcModLog)
runGhcMod path options ghcmod =
    bracket getCurrentDirectory setCurrentDirectory (const run)
  where
    dir = takeDirectory path
    run = setCurrentDirectory dir >> runGhcModT options ghcmod
