module Utils

open System
open System.Linq

let readLines filePath = System.IO.File.ReadLines(filePath)

let readLineInt filePath = 
    readLines filePath
    |> Seq.map Int32.Parse
                     
let print text = printf  "%s\n" text

let printn text = printf  "%i\n" text

let intersect (xs:'a seq) (ys: 'a seq) = xs.Intersect(ys)

let readCSV filePath = 
    let line = readLines filePath
               |> Seq.head
    line.Split [|','|] 
    |> Array.map Int32.Parse 
    |> Seq.toArray

let split (input: string) = 
    input.Split [|','|] 
    |> Seq.toArray
