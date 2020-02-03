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

  //  let libby = "supercalifragalisticxpalidocious"

    print "Part 1"
    let input = "12345678"
    let input = "80871224585914546619083218645595"
    let input = "19617804207202209144916044189917"
    let input =  readLines "./data/day16.txt" |> Array.head
    let input = "69317163492948606335995924319873"
      
    let o = iterate generate input 99
    printf "%A\n" (o.Substring(0,8))


    print "Part 2"

    let input2 = "02935109699940807407585447034323" // becomes 78725270.
    let input2 = "03081770884921959731165446850517" // becomes 53553731.
    let input2 = "03036732577212944063491565474664" // becomes 84462026.
    let input2 =  readLines "./data/day16.txt" |> Array.head
    let offset = input2.Substring(0, 7)
    let total = (input2.Length * 10000) - Int32.Parse offset

    let iseq = seq {
        let rev = input2 |> Seq.rev |> Seq.toArray
        for i in [0 .. total - 1] do
            yield Int32.Parse (rev.[i % input2.Length].ToString())
    }

    let phase ins =
        let mutable x = 0
        seq {
            for i in ins do
                x <- (x + i) % 10
                yield x
        }

    let p = iterate phase iseq 99

    let last n xs = xs |> Seq.skip (Seq.length xs - n)

    let p' = last 8 (p |> Seq.toList |> List.toSeq)

    let a = p' |> Seq.fold(fun a e -> e.ToString() + a) ""
    
    print a
        
    0