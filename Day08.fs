module Day8

open Utils

let day8 = 
    print "Advent of code - Day 8 - Space Image Format"

    let input = (readLines "./data/day8.txt").[0]

    let layer = 25 * 6

    let count x = Seq.filter ((=) x) >> Seq.length // **

    let layers image =
        image
        |> Seq.chunkBySize layer
        |> Seq.map (fun x -> (x |> count '0' , x))
        |> Seq.minBy fst
        
    let lowest = snd (layers input)

    let ones = lowest |> Array.filter (fun x -> x = '1')  |> Array.length
    let twos = lowest |> Array.filter (fun x -> x = '2')  |> Array.length
    


    printf "%A\n" (ones * twos)

    0