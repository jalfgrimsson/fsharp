#load "lib/FsLab.fsx"
open FsLab
open Foogle
open Deedle
open FSharp.Data

// ------------------------------------------------------------------
// WorldBank type provider
// ------------------------------------------------------------------

// Use the WorldBank type provider to find the capital cities of the
// smallest countries in the world. You can do this "by hand" - iterate
// over all the countries in the world and print the capitals of 
// countries smaller than 50000.0. 
//
// The following example prints the names of all countries:

let wb = WorldBankData.GetDataContext()
for c in wb.Countries do 
    printfn "%s" c.Name

// Given a country "c" you can access the value of an indicator in
// a specific year using the following:

let lithuania = wb.Countries.Lithuania
let gini = lithuania.Indicators.``GINI index``.[2000]
printfn "Gini: %f" gini


// (...)


// ------------------------------------------------------------------
// Plotting the World Population in 2000 by country
// ------------------------------------------------------------------
//

// Now, for each country in the world, retrieve the 
// sequence of all country names and their population
// in 2000, and map it into a tuple (name,population).

// Foogle.Chart allows you to directly feed a sequence
// and map country values by country name onto a world map.
// The following will plot the Example values for the 4 countries
// listed in the dataframe:

let seqExample = 
    [   ("China", 10.0)
        ("Canada", 20.0)
        ("France", 30.0)
        ("Germany", 40.0)
    ]

Chart.GeoChart(seqExample)


// (...)


// ------------------------------------------------------------------
// Using a Deedle data frame to store data
// ------------------------------------------------------------------
//
// Now, create a Deedle dataframe, with a series
// "Pop2000", containing for every country in the world
// the country name, and its population in 2000.
// 
// Foogle.Chart allows you to directly feed a dataframe
// and map country values by country name onto a world map.
// The following will compute the variance of the series,
// and plot the Example values for the 4 countries
// listed in the dataframe:

let example = series [ "China" => 10.; "Canada" => 20.; "France" => 30.; "Germany" => 40.; ]
example |> Stats.variance |> printfn "Variance: %.2f"
let geoExample = frame [ "Example" => example ]
Chart.GeoChart(geoExample, "Example")


// (...)


// ------------------------------------------------------------------
// Plotting the Population growth from 2000 to 2010
// ------------------------------------------------------------------
//
// Now, add to your existing frame the country population
// in 2010 as a new series, and a new column computing
// the percentage growth between 2000 and 2010.
// 
// You can create new series in an existing frame
// by combining them with standard operators, as in
// the following example:
//
let otherExample = series [ "China" => 100.; "Canada" => 200.; "France" => 300.; "Germany" => 400.; ]
geoExample?MoreData <- otherExample
geoExample?Combined <- geoExample?Example + geoExample?MoreData
Chart.GeoChart(geoExample, "Combined")


// (...)


// ------------------------------------------------------------------
// JSON Type Provider
// ------------------------------------------------------------------

// Using the JSON Type Provider, find the current temperature
// in Berlin (Germany).
// The open weather map API provides access to the temperature
// for cities in the world, via a JSON service call:
//
// http://api.openweathermap.org/data/2.5/weather?units=metric&q=Paris,France
//
// The JSON Type Provider allows you to generate types
// based on sample output from a JSON service. In a fashion similar
// to the CSV type provider, use the JsonProvider<...> where 
// ... is the service url to create a type T. Then, use T.Load(...)
// to call the service, passing in the appropriate parameters.
//
//     type Demo = JsonProvider<"http://sample.json">
//     let data = Demo.Load("http://data.json")
//     data. (...)
//


// (...)


// Next, grab the temperature for the capital city of every 
// country listed in the dataframe, and plot it on the map.
// First, write a small function retrieving the temperature
// for a specific city / country; given the fickle nature of
// the internet, wrapping it in a try / with block, returning
// nan (Not a number) in case of failure is advised.
//
// In F#, exception handling block can be written like this:
//
//     try 
//       failwith "oops"
//       42.0
//     with e -> -1.0
//
// To get the correct city, create a URL containing both the city
// and the country, for example "...&q=Berlin,Germany"
//
// To make the code a bit simpler, you can write a function 
// 'getTemp' that takes the country name & capital name and returns
// the temperature. You'll need something like this:
//
//     let getTemp country city = 
//       let url = "http://..." + country + "," + city
//       try
//         (...)
//       with _ -> (...)


// (...)