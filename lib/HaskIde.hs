{-# LANGUAGE OverloadedStrings #-}

module HaskIde where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as B
import           Data.Monoid ((<>))
import           System.FilePath (takeDirectory)

import           IdeSession
import           Neovim

testIde :: Neovim' String
testIde = do
    Right (ObjectBinary bspath) <- wait (vim_call_function "expand" [ObjectString "%:p"])
    let path = B.unpack bspath

    liftIO $ do
        -- Init for HEAD
        --config <- sessionConfigFromEnv
        sess <- initSession defaultSessionInitParams defaultSessionConfig
            { configLocalWorkingDir = Just "/Users/jake/src/ddc/ddc/packages/ddc-core-llvm/DDC" }

        -- Init for Hackage v0.9.0.11
        --sess <- initSession defaultSessionInitParams defaultSessionConfig

        let upd = -- updateSourceFile "Main.hs" "main = putStrLn \"Hello World\""
                  updateCodeGeneration False
               <> updateGhcOpts ["-Wall"]
        updateSession sess mempty (\progress -> return ())

        -- Print errors and warnings
        errs <- getSourceErrors sess
        let x1 = map show errs

        -- fs <- getManagedFiles sess
        -- let x1 = [show fs]

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
        let x2 = map show $ expTypes "DDC.Core.Llvm.Convert" SourceSpan
                 { spanFilePath = "/Users/jake/src/ddc/ddc/packages/ddc-core-llvm/DDC/Core/Llvm/Convert.hs"
                 , spanFromLine = 47
                 , spanFromColumn = 25
                 , spanToLine = 47
                 , spanToColumn = 25
                 }

        -- Autocompletion
        autoCompletion <- getAutocompletion sess
        let x3 = map show $ autoCompletion "DDC.Core.Llvm.Convert" "nameGlobal"

        return $ unlines (x1 ++ x2 ++ x3)
