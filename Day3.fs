module Day3

open Utils

let day3 = 
    print "Advent of code - Day 3"

    let input = readLines "./data/day3.txt"

    let wire1 = input |> Seq.head |> Utils.split
    let wire2 = input |> Seq.skip 1 |> Seq.head |> Utils.split

    let route start vector = 
        let splitv (dir:string) = 
            (dir.[0], dir.[1..] |> System.Int32.Parse)
        let inst = splitv vector
        match fst inst with
        | 'U' -> seq { for v in snd start .. (snd start + snd inst) do  (fst start, v) }
        | 'D' -> seq { for v in snd start .. -1 .. (snd start - snd inst) do (fst start, v) }
        | 'L' -> seq { for v in fst start .. -1 .. (fst start - snd inst) do (v, snd start) }
        | 'R' -> seq { for v in fst start .. (fst start + snd inst) do  (v, snd start) }
        | _ -> Seq.empty

    let s = seq { yield (0,0)}

    let folder acc elem = 
        let start = acc |> Seq.last
        let r = route start elem
        Seq.append acc (r |> Seq.skip 1)

    let coords wire =
        wire
        |> Seq.fold folder s

    
    let x = coords wire1
    let y = coords wire2

    let cross = intersect x y
   

    let manhatten = cross   
                    |> Seq.skip 1
                    |> Seq.map (fun x -> abs (fst x) + abs (snd x))
                    |> Seq.min
    
    printf "%A\n" manhatten

    ()