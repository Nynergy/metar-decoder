module Main where

import System.Environment

import Scraper

main :: IO ()
main = do
  args <- getArgs
  if (args == [])
    then putStrLn "USAGE: metar-decoder [STATION ID]"
    else do
      code <- getCode $ head args
      case code of
        Left errorMsg -> putStrLn errorMsg
        Right metar   -> putStrLn metar -- For the moment we just spit out the code
