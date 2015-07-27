{-# LANGUAGE OverloadedStrings #-}

module HaskIde where

import qualified Data.ByteString.Char8 as B
import           Data.Monoid ((<>))
import           Control.Monad.IO.Class (liftIO)

import           IdeSession
import           Neovim

testIde :: Neovim' String
testIde = liftIO $ do
    -- Init for HEAD
    config <- sessionConfigFromEnv
    sess <- initSession defaultSessionInitParams config
        { configLocalWorkingDir = Nothing }

    -- Init for Hackage v0.9.0.11
    --sess <- initSession defaultSessionInitParams defaultSessionConfig

    let upd = updateSourceFile "Main.hs" "main = putStrLn \"Hello World\""
           <> updateCodeGeneration True
           <> updateGhcOpts ["-Wall"]
    updateSession sess upd (\progress -> return ())

    -- Print errors and warnings
    errs <- getSourceErrors sess
    let x1 = map show errs

    -- Run the code
    --ra <- runStmt sess "Main" "main"
    --let loop = do
    --        res <- runWait ra
    --        case res of
    --            Left  bs -> B.putStr bs >> loop
    --            Right rr -> putStrLn $ "Run result: " ++ show rr
    --loop

    -- Get some type information
    expTypes <- getExpTypes sess
    let x2 = map show $ expTypes "Main" SourceSpan
             { spanFilePath = "Main.hs"
             , spanFromLine = 1
             , spanFromColumn = 8
             , spanToLine = 1
             , spanToColumn = 9
             }

    -- Autocompletion
    autoCompletion <- getAutocompletion sess
    let x3 = map show $ autoCompletion "Main" "putS"

    return $ unlines (x1 ++ x2 ++ x3)
