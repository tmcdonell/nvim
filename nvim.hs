{-# LANGUAGE TemplateHaskell #-}

import           Neovim

import qualified Data.Map as Map
import           System.Random

import           GhcModPlugin
--import           HaskIde
import           MyFirstPlugin (fibonacci)
import           RandomPlugin

------------------------------------------------------------------------

ghcModPlugin :: IO NeovimPlugin
ghcModPlugin = do
    wrapPlugin Plugin {
      exports         = []
    , statefulExports = [ ((), emptyState, [ $(function' 'testGhcMod) Sync ]) ]
    }

------------------------------------------------------------------------

--idePlugin :: IO NeovimPlugin
--idePlugin = wrapPlugin Plugin {
--      exports         = [ $(function' 'testIde) Sync ]
--    , statefulExports = []
--    }

------------------------------------------------------------------------

fibonacciPlugin :: IO NeovimPlugin
fibonacciPlugin = wrapPlugin Plugin {
      exports         = [ $(function' 'fibonacci) Sync ]
    , statefulExports = []
    }

------------------------------------------------------------------------

randomPlugin :: IO NeovimPlugin
randomPlugin = do
    g <- newStdGen                -- initialize with a random seed
    let randomNumbers = randoms g -- an infinite list of random numbers
    wrapPlugin Plugin {
      exports         = []
    , statefulExports = [ ((), randomNumbers,
                          [ $(function' 'nextRand)      Sync
                          , $(function' 'setNextNumber) Async
                          ])]
    }

------------------------------------------------------------------------

main :: IO ()
main = neovim def { plugins = [fibonacciPlugin, randomPlugin, ghcModPlugin] }
