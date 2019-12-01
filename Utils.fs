module Utils

open System

let readLines filePath = System.IO.File.ReadLines(filePath)

let readLineInt filePath = 
    readLines filePath
    |> Seq.map Int32.Parse
                     
let print text = printf  "%s\n" text

let printn text = printf  "%i\n" text



