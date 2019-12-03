module Day3

open Utils

let day3 = 
    print "Advent of code - Day 3"

    let input = readLines "./data/day3.txt"
    
    let wire1 = input |> Seq.head |> Utils.split
    let wire2 = input |> Seq.skip 1 |> Seq.head |> Utils.split


// elem R991,U77

    let splitv (dir:string) = 
        (dir.[0], dir.[1..] |> System.Int32.Parse)
        //        (340, 430)  L43
    let route start vector = 
        let inst = splitv vector
        match fst inst with
        | 'U' -> seq { for v in snd start .. (snd start + snd inst) do  (fst start, v) }
        | 'D' -> seq { for v in snd start .. (snd start - snd inst) do  (fst start, v) }
        | 'L' -> seq { for v in fst start .. (fst start - snd inst) do  (v, snd start) }
        | 'R' -> seq { for v in fst start .. (fst start + snd inst) do  (v, snd start) }
        | _ -> seq { (0,0) }


    // let coords wire =
    //     wire
    //     |> Seq.windowed 2
    //     |> Seq.fold (fun acc elem -> printf elem.[0]) (0,0)

    let x = route (0,0) "L43"
    
    




    //let coords1 = coords wire1
    //let coords1 = coords wire1
    






    ()