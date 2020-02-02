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
  
    print "Part 1"
    let input = "12345678"
    let input = "80871224585914546619083218645595"
    let input = "19617804207202209144916044189917"
    let input = "69317163492948606335995924319873"
    let input =  readLines "./data/day16.txt" |> Array.head
      
    let o = iterate generate input 99
    printf "%A\n" (o.Substring(0,8))

    print "Part 2"

    0