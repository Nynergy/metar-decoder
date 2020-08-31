module Parser where

import Control.Applicative
import Text.ParserCombinators.ReadP

import MetarTypes

-- | Helper Functions

isUpperCase :: Char -> Bool
isUpperCase char = char >= 'A' && char <= 'Z'

isDigit :: Char -> Bool
isDigit char = char >= '0' && char <= '9'

-- | Parsers

numbers :: Int -> ReadP Int
numbers digits = fmap read (count digits digit)

digit :: ReadP Char
digit = satisfy isDigit

report :: ReadP Report
report = do
  station <- stationCode
  time    <- timeCode
  auto    <- automatedCode
  cor     <- correctionCode
  wind    <- option Nothing (fmap Just windInfo)
  vis     <- visibilityInfo
  weather <- option Nothing (fmap Just weatherInfo)
  clouds  <- cloudInfo
  return (Report
          station
          time
          auto
          cor
          wind
          vis
          weather
          clouds)

stationCode :: ReadP StationCode
stationCode = do
  code <- many1 (satisfy isUpperCase)
  string " "
  return code

timeCode :: ReadP TimeCode
timeCode = do
  day    <- numbers 2
  hour   <- numbers 2
  minute <- numbers 2
  string "Z "
  if isValidDay day && isValidHour hour && isValidMinute minute
    then return (TimeCode day hour minute)
    else pfail
    where
      isValidDay day       = day >= 1 && day <= 31
      isValidHour hour     = hour <= 23
      isValidMinute minute = minute <= 59

automatedCode :: ReadP Bool
automatedCode = do
  auto <- option Nothing (fmap Just (string "AUTO"))
  case auto of
    Nothing -> return False
    Just _  -> do
      string " "
      return True

correctionCode :: ReadP Bool
correctionCode = do
  cor <- option Nothing (fmap Just (string "COR"))
  case cor of
    Nothing -> return False
    Just _  -> do
      string " "
      return True

windInfo :: ReadP WindInfo
windInfo = do
  direction <- (fmap Left $ string "VRB") <|> (fmap Right $ numbers 3)
  speed     <- numbers 2 <|> numbers 3
  gusts     <- option Nothing (fmap Just gustParser)
  unit      <- string "KT" <|> string "MPS"
  string " "
  variable  <- option Nothing (fmap Just variableWindParser)
  return (WindInfo
    direction
    speed
    gusts
    unit
    variable)

gustParser :: ReadP Int
gustParser = do
  string "G"
  gust <- numbers 2 <|> numbers 3
  return gust

variableWindParser :: ReadP VariableWind
variableWindParser = do
  low  <- numbers 3
  string "V"
  high <- numbers 3
  string " "
  return (VariableWind low high)

visibilityInfo :: ReadP VisibilityInfo
visibilityInfo = do
  distance <- fractionalDistParser <|> wholeDistParser
  string "SM "
  runways  <- option Nothing (fmap Just runwaysParser)
  return (VisibilityInfo distance runways)

fractionalDistParser :: ReadP Float
fractionalDistParser = do
  first  <- numbers 1 <|> numbers 2
  string "/"
  second <- numbers 1 <|> numbers 2
  let num = (fromIntegral first) / (fromIntegral second)
  return num

wholeDistParser :: ReadP Float
wholeDistParser = do
  num <- numbers 1 <|> numbers 2
  return (fromIntegral num)

runwaysParser :: ReadP [RunwayVis]
runwaysParser = do
  many1 runwayParser

runwayParser :: ReadP RunwayVis
runwayParser = do
  string "R"
  runwayNum   <- numbers 1 <|> numbers 2
  orientation <- option Nothing (fmap Just $ char 'L' <|> char 'C' <|> char 'R')
  string "/"
  distance    <- (fmap Left variableRunwayVis) <|> (fmap Right $ numbers 4)
  string "FT "
  return (RunwayVis
          runwayNum
          orientation
          distance)

variableRunwayVis :: ReadP VariableVis
variableRunwayVis = do
  min    <- numbers 4
  string "V"
  prefix <- option Nothing (fmap Just $ char 'P' <|> char 'M')
  max    <- numbers 4
  return (VariableVis
          min
          prefix
          max)

weatherInfo :: ReadP WeatherInfo
weatherInfo = do
  intensity <- option Nothing (fmap Just (char '-' <|> char '+'))
  proximity <- option Nothing (fmap Just $ string "VC")
  desc      <- option Nothing (fmap Just weatherDescParser)
  phenoms   <- option Nothing (fmap Just $ phenomsParser)
  string " "
  return (WeatherInfo
          intensity
          proximity
          desc
          phenoms)

weatherDescParser :: ReadP String
weatherDescParser = do
  desc <- string "BC" <|> string "BL" <|> string "DR"
                      <|> string "FZ" <|> string "MI"
                      <|> string "PR" <|> string "SH"
                      <|> string "TS"
  return desc

phenomsParser :: ReadP [String]
phenomsParser = do
  phenoms <- many1 phenomParser
  return phenoms

phenomParser :: ReadP String
phenomParser = do
  phenom <- string "DZ" <|> string "GR" <|> string "GS" <|> string "IC"
                        <|> string "PL" <|> string "RA" <|> string "SG"
                        <|> string "SN" <|> string "UP" <|> string "BR"
                        <|> string "DU" <|> string "FG" <|> string "FU"
                        <|> string "HZ" <|> string "PY" <|> string "SA"
                        <|> string "VA" <|> string "DS" <|> string "FC"
                        <|> string "PO" <|> string "SQ" <|> string "SS"
  return phenom

cloudInfo :: ReadP CloudInfo
cloudInfo = do
  groups <- many1 cloudGroupParser
  return (CloudInfo groups)

cloudGroupParser :: ReadP CloudGroup
cloudGroupParser = do
  coverage <- string "CLR" <|> string "FEW" <|> string "SCT"
                           <|> string "BKN" <|> string "OVC"
  case coverage of
    "CLR" -> do
      let base = 0
      let convec = Nothing
      string " "
      return (CloudGroup
              coverage
              base
              convec)
    _     -> do
      base     <- fmap (*100) $ numbers 3
      convec   <- option Nothing (fmap Just (string "CB" <|> string "TCU"))
      string " "
      return (CloudGroup
              coverage
              base
              convec)
