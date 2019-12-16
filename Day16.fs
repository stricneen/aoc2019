module Day16

open System
open Utils

let day16 =
    print "Advent of code - Day 16 - Flawed Frequency Transmission"

    


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

    let digit input i =
        Seq.map2 (fun a b -> a, b) input (pattern i)
        |> Seq.sumBy(fun (a, b) -> int(a.ToString()) * b)

    let generate inp =
        inp
        |> Seq.mapi (fun i _ -> (digit inp i).ToString())
        |> Seq.map (fun x -> x.[x.Length - 1])
        |> String.Concat
  

    // let input = "69317163492948606335995924319873"

    let input =  readLines "./data/day16.txt" |> Array.head
    
    
    iterate (fun x ->   printf "%A\n" x
                        generate x) input 100








    0