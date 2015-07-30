{-# LANGUAGE TemplateHaskell #-}

import Neovim

import System.Random

import MyFirstPlugin (fibonacci)
import RandomPlugin
import GhcModPlugin
--import HaskIde

------------------------------------------------------------------------

ghcModPlugin :: IO NeovimPlugin
ghcModPlugin = do
    wrapPlugin Plugin {
      exports         = []
    , statefulExports = [ ((), Nothing, [ $(function' 'testGhcMod) Sync ]) ]
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
