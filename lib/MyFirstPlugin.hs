module MyFirstPlugin
    ( fibonacci
    ) where

import Neovim

fibonacci :: Int -> Neovim' String
fibonacci n = return (show $ fibs !! n)
  where
    fibs :: [Integer]
    fibs = 1:1:scanl1 (+) fibs
