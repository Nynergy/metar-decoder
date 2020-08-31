module MetarTypes where

type MetarCode = String

data Report = Report
  { station :: StationCode
  , time :: TimeCode
  , auto :: Bool
  , correction :: Bool
  , wind :: Maybe WindInfo
  , vis :: VisibilityInfo
  , weather :: Maybe [WeatherInfo]
  , clouds :: CloudInfo
  , dewtemp :: DewTempInfo
  , altimeter :: Int
  } deriving (Show)

type StationCode = String

data TimeCode = TimeCode
  { day :: Int
  , hour :: Int
  , minute :: Int
  } deriving (Show)

data WindInfo = WindInfo
  { dir :: Either String Int
  , speed :: Int
  , gusts :: Maybe Int
  , unit :: String
  , variable :: Maybe VariableWind
  } deriving (Show)

data VariableWind = VariableWind
  { low :: Int
  , high :: Int
  } deriving (Show)

data VisibilityInfo = VisibilityInfo
  { general :: Float
  , runways :: Maybe [RunwayVis]
  } deriving (Show)

data RunwayVis = RunwayVis
  { runway :: Int
  , orient :: Maybe Char
  , dist :: Either VariableVis Int
  } deriving (Show)

data VariableVis = VariableVis
  { min :: Int
  , prefix :: Maybe Char
  , max :: Int
  } deriving (Show)

data WeatherInfo = WeatherInfo
  { intensity :: Maybe Char
  , proximity :: Maybe String
  , description :: Maybe String
  , phenoms :: Maybe [String]
  } deriving (Show)

data CloudInfo = CloudInfo
  { groups :: [CloudGroup]
  } deriving (Show)

data CloudGroup = CloudGroup
  { coverage :: String
  , base :: Int
  , convective :: Maybe String
  } deriving (Show)

data DewTempInfo = DewTempInfo
  { temp :: Int
  , dew :: Int
  } deriving (Show)
