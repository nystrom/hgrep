module Main where

import Lib
import System.Environment (getArgs)
import System.IO (isEOF)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [arg] -> parseAndGrep arg
    _ -> usage

usage :: IO ()
usage = putStrLn "usage: hgrep <regex>"

parseAndGrep :: String -> IO ()
parseAndGrep pat = do
  case parseRegex pat of
     Just r -> grep r
     Nothing -> putStrLn $ "invalid regular expression " ++ pat

grep :: Regex -> IO ()
grep r = go
  where
    go = do
      done <- isEOF
      if done
        then return ()
        else do
          line <- getLine
          if matchLine r line
            then do
              putStrLn line
              go
            else go

