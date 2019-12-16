module Day16

open System
open Utils

let day16 =
    print "Advent of code - Day 16 - Flawed Frequency Transmission"

    let prog =  readLines "./data/day16.txt" |> Array.head

    let input = "12345678"

    let countSeq p =
        seq {
            yield! seq { for _ in 0 .. p ->  0 }
            yield! seq { for _ in 0 .. p ->  1 }
            yield! seq { for _ in 0 .. p ->  0 }
            yield! seq { for _ in 0 .. p -> -1 }
        }

    // let inputSeq = input |> Seq.toSeq

    let rec cycle xs = seq { yield! xs; yield! cycle xs }

    let pattern p = cycle (countSeq p) |> Seq.skip 1

    // let x =
    //     Seq.initInfinite pattern
    //     |> Seq.zip input

    let digit i =
        Seq.map2 (fun a b -> a, b) input (pattern i)
        |> Seq.sumBy(fun (a, b) -> int(a.ToString()) * b)

    //for i in 0 .. String.length input  do


    let generate inp =
        inp
        |> Seq.mapi(fun i _ -> (digit i).ToString())
        |> Seq.map (fun x -> x.[x.Length - 1])
        |> String.Concat
  
    let next = generate input

    printf "%A\n" next






    0