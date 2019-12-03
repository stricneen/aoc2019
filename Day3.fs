module Day3

open Utils
open System.Linq

let day3 = 
    print "Advent of code - Day 3"

    let input = readLines "./data/day3.txt"
    
    let wire1 = input |> Seq.head |> Utils.split
    let wire2 = input |> Seq.skip 1 |> Seq.head |> Utils.split


        //        (340, 430)  L43
    let route start vector = 
        let splitv (dir:string) = 
            (dir.[0], dir.[1..] |> System.Int32.Parse)
        let inst = splitv vector
        match fst inst with
        | 'U' -> seq { for v in snd start .. (snd start + snd inst) do  (fst start, v) }
        | 'D' -> seq { for v in snd start .. -1 .. (snd start - snd inst) do (fst start, v) }
        | 'L' -> seq { for v in fst start .. -1 .. (fst start - snd inst) do (v, snd start) }
        | 'R' -> seq { for v in fst start .. (fst start + snd inst) do  (v, snd start) }
        | _ -> seq { (0,0) }

    let s = seq { yield (0,0)}

    let folder acc elem = 
        let start = acc |> Seq.last
        let r = route start elem
        acc 
        |> Seq.append r

    let coords wire =
        wire
        |> Seq.fold folder s

    let y = route (0,0) "L43"
    
    let x = coords wire1
    let y = coords wire2

    let cross = intersect x y

    printf "%A" cross
    
    //y |> Seq.iter (fun x -> printf "%a , %a" (fst x) (snd x))
    
    //printn (y |> Seq.length)



    print "end"

    //let coords1 = coords wire1
    //let coords1 = coords wire1
    






    ()