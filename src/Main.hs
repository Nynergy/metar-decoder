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
              let (decoded, rest) = metarInfo !! (length metarInfo - 1)
              print decoded
              print $ "Rest of input: " ++ rest
            _                 -> do
              print $ "Could not parse code: " ++ metar
