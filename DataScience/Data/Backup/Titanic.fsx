#load "lib/FsLab.fsx"
open FsLab
open Deedle
open FSharp.Data

(* Type provider demo: CSV *)

type Titanic = CsvProvider<"Titanic.csv">
let data = Titanic.GetSample()
let first = data.Rows |> Seq.head

(* Using Sequences *)

let tupled = 
    data.Rows
    |> Seq.filter (fun x -> x.Sex = "female")
    |> Seq.averageBy (fun x -> x.Fare)

(* Using Deedle series *)

let fare = series [ for p in data.Rows -> p.PassengerId => float p.Fare ]
fare |> Stats.median

let frame = frame [ "Fare" => fare ]

let passClass = series  [ for p in data.Rows -> p.PassengerId => p.Pclass ]
frame?Class <- passClass

frame