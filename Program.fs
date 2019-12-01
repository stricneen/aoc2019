// Learn more about F# at http://fsharp.org

open System
open Utils

[<EntryPoint>]
let main argv =
    print "Advent of code - Day 1"

    // Part 1

    let numbers = readNumberLine "./data/day1.txt"
                  |> Seq.sumBy (fun x -> (x / 3) - 2)

    printn numbers // 3394689
    
    // Part 2

    let rec fuelreq mass = seq {
        let c = (mass / 3) - 2
        match c with
        | n when n < 1 -> ()
        | _ -> yield c
               yield! fuelreq c }

    let massAndFuel = readNumberLine "./data/day1.txt"
                       |> Seq.sumBy (fun x -> (fuelreq x |> Seq.sum) )

    printn massAndFuel // 5089160

    0 // return an integer exit code
