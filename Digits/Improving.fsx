open System
open System.IO

type Example = { Label:int; Pixels:int[] }

let dropHeaders (data:string[]) = data.[1..]

#time

let readData (path:string) =
    File.ReadAllLines(path)
    |> dropHeaders
    |> Array.Parallel.map (fun line -> line.Split(','))
    |> Array.Parallel.map (fun line -> line |> Array.map int)
    |> Array.Parallel.map (fun line -> { Label = line.[0]; Pixels = line.[1..] })

let trainingPath =  __SOURCE_DIRECTORY__ + @"\trainingsample.csv"
let validationPath = __SOURCE_DIRECTORY__ + @"\validationsample.csv"

let training = readData trainingPath
let validation = readData validationPath 

type Pixels = int []

let distance (pix1:Pixels) (pix2:Pixels) =
    (pix1, pix2) 
    ||> Array.map2 (fun x1 x2 -> pown (x1 - x2) 2)
    |> Array.sum

let classify (pix:Pixels) distfun =
    training
    |> Array.minBy (fun ex -> distfun pix ex.Pixels)
    |> fun ex -> ex.Label

let quality (sample:Example[]) distfun =
    sample
    |> Array.map (fun ex -> 
        if classify (ex.Pixels) distfun = ex.Label then 1. else 0.)
    |> Array.average
 

let createDistFunc2 =
    let dist = fun (pix1:Pixels) (pix2:Pixels) ->
        let adjusted = [|
            for x in 0..Array.length(pix1) do
                let subtotal = pix1.[x] - pix2.[x]
                yield pown subtotal 2
         |]
        adjusted |> Array.sum
    dist

let createDistFunc power =
    let dist = fun (pix1:Pixels) (pix2:Pixels) ->
        (pix1, pix2) 
        ||> Array.map2 (fun x1 x2 -> pown ((float x1)/(float power) - (float x2)/(float power)) 2)
        |> Array.sum
    dist

//let qualities = [|2..2|] |> Array.map (fun x -> quality validation (createDistFunc2 x))
//qualities |> Array.map (fun x-> printfn "%.4f" x)

let dist2 (pix1:Pixels) (pix2:Pixels) =
    (pix1, pix2) 
    ||> Array.map2 (fun x1 x2 -> abs (x1 - x2))
    |> Array.sum


let dist = fun (pix1:Pixels) (pix2:Pixels) ->
        let adjusted = [|
            for x in 0..(Array.length(pix1)-1) do
                let subtotal = pix1.[x] - pix2.[x]
                yield pown subtotal 2
         |]
        adjusted |> Array.sum
    

printfn "Quality: %.4f" (quality validation dist2)


#time

(*
Improving the model: multiple possibilities

Improving speed 

Check Array.Parallel, and #time

Improving quality: some possible directions

1. Use a different distance
   ex: d = abs(x1-y1) + ... abs(xk-yk)
   ex: d = abs(x1-y1)^3 + ...

2. Instead of 1 neighbor, use k neighbors 
and take a majority vote. What is the best k? 

3. "reduce noise": instead of 0 to 255, reduce
pixels encoding to pix/2, pix/3, ...
What is the best value?

4. "reduce noise": instead of comparing individual
pixels, compare square blocks of pixels: 2x2, 3x3, ...
What is the best value?


*)

let size = 28
let reduce (pix:Pixels) (width:int) =
    [| 
        for row in 0 .. (size - width) do
            for col in 0 .. (size - width) do
                let total =
                    [
                        for r in 0 .. (width - 1) do
                            for c in 0 .. (width - 1) do
                                yield pix.[(row + r) * size + col + c]
                    ]
                    |> List.sum
                    |> fun x -> x / width
                yield total
    |]