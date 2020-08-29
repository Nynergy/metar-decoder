module Scraper where

import Control.Lens
import Data.List
import Data.List.Split
import Network.Wreq

import MetarTypes

getCode :: StationCode -> IO (Either String MetarCode)
getCode code = do
  let opts = defaults & param "format" .~ ["raw"]
                      & param "date" .~ ["0"]
                      & param "hours" .~ ["0"]
  src <- getWith opts ("https://aviationweather.gov/metar/data?ids=" ++ code)
  let body = src ^. responseBody
  -- Ideally there should only be one code, but we take the head anyway
  let metarCodes = filter isCodeLine $ splitOn "\\n" (show body)
  if (metarCodes /= [])
    then return (Right $ stripCode $ head metarCodes)
    else return (Left $ "Could Not Find Metar Code for Station " ++ code ++ ".")

isCodeLine :: String -> Bool
isCodeLine str = "<code>" `isInfixOf` str && "</code>" `isInfixOf` str

stripCode :: String -> String
stripCode str = takeWhile (/= '<') $ drop 1 $ dropWhile (/= '>') str
