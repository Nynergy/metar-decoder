# metar-decoder

A command line tool for scraping and decoding METAR codes. This program is still a work in progress, and all it does right now is take a standard ICAO code for a station (e.g. KJFK for JFK in New York), scrape the current METAR code from the [Aviation Weather website](https://aviationweather.gov/metar), and present it to the user on the console. You can check that website for a list of other ICAO codes to use.

In the future, I will be implementing a parser that will parse the code and extract the weather and runway conditions from it. This information will then be presented to the user in a more human-readable format.

--------------------------------------------------------------------------------

## Installation

Use `stack` to install. The following code should be sufficient as long as your `~/.local/bin` directory is in your user `$PATH` variable. If this directory is not in your `$PATH`, you won't be able to run the program unless you give the absolute path every time you want to run it.

```
git clone https://github.com/Nynergy/metar-decoder.git
cd metar-decoder
stack build
stack install metar-decoder
```

--------------------------------------------------------------------------------

## Usage

To use the program, simply call `metar-decoder [STATION ID]` where the station ID is the ICAO airport code you wish to get information for.
