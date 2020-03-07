module Utils

open System
open System.Linq

let readLines filePath = System.IO.File.ReadAllLines(filePath)

let readLineInt filePath = 
    readLines filePath
    |> Seq.map Int32.Parse
                     
let print text = printf  "%s\n" text

let printn text = printf  "%i\n" text

let pl lg = printf "%A\n" lg

let rec printmap lst = 
    //printf "\t%A\n" "0123456789012345678901234"
    for x in 0 .. Array2D.length1 lst - 1 do
        let l = lst.[x,*] |> String
        printf "%A\t%A\n" x l
    print ""

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
    match count with
    | 0 -> output
    | _ -> iterate func output (count - 1)
    
let printAt x y (message: string) = 
    Console.SetCursorPosition(x, y)
    Console.Write(message)
