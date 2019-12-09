module Day3

open Utils

// Wires

let day3 = 
    print "Advent of code - Day 3 - Crossed Wires"

    let input = readLines "./data/day3.txt"

    let wire1 = input |> Seq.head |> Utils.split
    let wire2 = input |> Seq.skip 1 |> Seq.head |> Utils.split

    let addlength start vector = 
        let splitv (dir:string) = 
            (dir.[0], dir.[1..] |> System.Int32.Parse)
        let inst = splitv vector
        match fst inst with
        | 'U' -> seq { for v in snd start .. (snd start + snd inst) do  (fst start, v) }
        | 'D' -> seq { for v in snd start .. -1 .. (snd start - snd inst) do (fst start, v) }
        | 'L' -> seq { for v in fst start .. -1 .. (fst start - snd inst) do (v, snd start) }
        | 'R' -> seq { for v in fst start .. (fst start + snd inst) do  (v, snd start) }
        | _ -> Seq.empty


    let iter acc elem = 
        let start = acc |> Seq.last
        let r = addlength start elem
        Seq.append acc (r |> Seq.skip 1)

    let origin = seq { yield (0,0) }

    let route wire =
        wire
        |> Seq.fold iter origin

    
    let wire1Route = route wire1
    let wire2Route = route wire2

    let cross = intersect wire1Route wire2Route

    printf "Intersections : %A\n" (cross |> Seq.toList)
    printf "Length       : %A\n" (cross |> Seq.length)
    

    let manhatten = cross   
                    |> Seq.skip 1
                    |> Seq.map (fun x -> abs (fst x) + abs (snd x))
                    |> Seq.min
    
    printf "%A\n" manhatten


    print "Part 2"
    
    printf "W1 : %A\n" (wire1Route |> Seq.length)
    printf "W2 : %A\n" (wire1Route |> Seq.length)

    let cross1 = cross
                 |> Seq.skip 1
                 |> Seq.map (fun x -> wire1Route |> Seq.findIndex (fun y -> y = x ))
    let cross2 = cross
                 |> Seq.skip 1
                 |> Seq.map (fun x -> wire2Route |> Seq.findIndex (fun y -> y = x ))

    printf "W1 : %A\n" (cross1 |> Seq.toList)
    printf "W2 : %A\n" (cross2 |> Seq.toList)

    let zip = Seq.zip cross1 cross2 |> Seq.map (fun (x, y) -> x + y)

    let min = Seq.min zip
    printf "min : %A\n" min
    
    
    printf "%A\n" (cross |> Seq.skip 1 |> Seq.head)


    ()