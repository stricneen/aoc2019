module Utils


let readLines filePath = System.IO.File.ReadLines(filePath)

let readNumberLine filePath = 
    System.IO.File.ReadLines(filePath)
    |> Seq.map System.Int32.Parse
                     
let print text = printf  "%s\n" text

let printn text = printf  "%i\n" text



