module Day16

open System
open Utils

let day16 =
    print "Advent of code - Day 16 - Flawed Frequency Transmission"

    let prog =  readLines "./data/day16.txt" |> Array.head


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
  

    let input = "12345678"

    
    
    iterate (fun x ->   printf "%A\n" x
                        generate x) input 100

    // let n1 = generate input
    // printf "%A\n" n1
    
    // let n2 = generate n1
    // printf "%A\n" n2

    // let n3 = generate n2
    // printf "%A\n" n3

    // let n3 = generate n3
    // printf "%A\n" n3






    0