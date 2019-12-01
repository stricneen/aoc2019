// Learn more about F# at http://fsharp.org

open System
open Utils

[<EntryPoint>]
let main argv =
    print "Advent of code"


    // (mass / 3) - 2
    // let numbers = readNumberLine "./data/day1.txt"
    //                 |> Seq.map (fun x -> (x / 3) - 2)
    //                 |> Seq.sum

    let numbers = readNumberLine "./data/day1.txt"
                    |> Seq.sumBy (fun x -> (x / 3) - 2)
    

    printn numbers

    0 // return an integer exit code
