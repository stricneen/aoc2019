module Utils

open System
open System.Linq

let readLines filePath = System.IO.File.ReadAllLines(filePath)

let readLineInt filePath = 
    readLines filePath
    |> Seq.map Int32.Parse
                     
let print text = printf  "%s\n" text

let printn text = printf  "%i\n" text

let intersect (xs:'a seq) (ys: 'a seq) = xs.Intersect(ys)

let toSeq (str: string) =
    str.Split [|','|] 
    |> Array.map Int64.Parse 
    |> Seq.toArray

let readCSV filePath = 
    let line = readLines filePath
               |> Seq.head
    line.Split [|','|] 
    |> Array.map Int64.Parse 
    |> Seq.toArray

let split (input: string) = 
    input.Split [|','|] 
    |> Seq.toArray




let distrib e L = // ** 1526046
    let rec aux pre post = 
        seq {
            match post with
            | [] -> yield (L @ [e])
            | h::t -> yield (List.rev pre @ [e] @ post)
                      yield! aux (h::pre) t 
        }
    aux [] L

let rec perms = function 
    | [] -> Seq.singleton []
    | h::t -> Seq.collect (distrib h) (perms t)


let rec comb n l =   // ** 1222185
    match n, l with
    | 0, _ -> [[]]
    | _, [] -> []
    | k, (x::xs) -> List.map ((@) [x]) (comb (k-1) xs) @ comb k xs


let fst3 t = 
    let x, _, _ = t
    x
 
let snd3 t = 
    let _, x, _ = t
    x

let trd3 t = 
    let _, _, x = t
    x

let fst4 t = 
    let x, _, _, _ = t
    x


let rec iterate func input count =
    let output = func input

    //if count % 10 = 1 then  
    //print output

    match count with
    | 0 -> ()
    | _ -> iterate func output (count - 1)
    
let printAt x y (message: string) = 
    Console.SetCursorPosition(x, y)
    Console.Write(message)
