module Main where

import System.Environment
import Text.ParserCombinators.ReadP

import Parser
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
        Right metar   -> do
          let metarInfo = readP_to_S report metar
          case metarInfo of
            ((info, rest):xs) -> do
              print info
              print rest
              print "Other possible parses:"
              print xs
            _                 -> do
              print $ "Could not parse code: " ++ metar
