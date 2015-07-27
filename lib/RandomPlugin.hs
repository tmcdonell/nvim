module RandomPlugin
    ( nextRand
    , setNextNumber
    ) where

import Neovim

nextRand :: Neovim r [Int16] Int16
nextRand = do
    r <- gets head
    modify tail
    return r

setNextNumber :: Int16 -> Neovim r [Int16] ()
setNextNumber n = modify (n:)
