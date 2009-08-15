module Main where

{-# NOINLINE fn #-}
fn :: Int -> ((),Bool)
fn x = ((), case x of 0 -> True; _ -> False)

main :: IO ()
main = print (fst (fn undefined))
