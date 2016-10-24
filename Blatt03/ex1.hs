module Fibo where

fibs :: [Integer]
fibs = 0:1:1:zipWith (+) (tail fibs) (tail $ tail fibs)
