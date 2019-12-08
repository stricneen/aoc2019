module Day8

open Utils

let day8 = 
    print "Advent of code - Day 8 - Space Image Format"

    let input = (readLines "./data/day8.txt").[0]

    let pixels = 25 * 6

    let count x = Seq.filter ((=) x) >> Seq.length // ** 40022846

    let layers image =
        image
        |> Seq.chunkBySize pixels

    let photoLayers = layers input

    let lowestZeroCount image =
        image
        |> Seq.map (fun x -> (x |> count '0' , x))
        |> Seq.minBy fst
        
    let lowest = snd (lowestZeroCount photoLayers)

    let ones = lowest |> Array.filter (fun x -> x = '1')  |> Array.length
    let twos = lowest |> Array.filter (fun x -> x = '2')  |> Array.length

    printf "%A\n" (ones * twos)

    print "Part 2"

    let photo = 
        photoLayers
        |> Seq.transpose
        |> Seq.map (fun x -> x |> Seq.find (fun y -> y <> '2'))
        |> Seq.chunkBySize 25
        |> Seq.map System.String
        // |> Seq.map (fun x -> x.Replace('1', '*'))
        // |> Seq.map (fun x -> x.Replace('0', ' '))

    photo |> Seq.iter (printf "%A\n")

    0