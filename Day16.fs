module Day16

open Utils

let day16 = 
    print "Advent of code - Day 16 - Flawed Frequency Transmission"

    let prog =  readLines "./data/day16.txt" |> Array.head

    let test1 = "12345678"

    let countSeq p =
        seq {
            yield! seq { for _ in 0 .. p -> 0 }
            yield! seq { for _ in 0 .. p -> 1}
            yield! seq { for _ in 0 .. p -> 0 }
            yield! seq { for _ in 0 .. p -> -1 }
        }
            
    let rec cycle xs = seq { yield! xs; yield! cycle xs }

    let pattern p = cycle (countSeq p)

                // for x in 0..3 do
                //     print "S"

                //inc
           // inc
        


    // let pattern position = 
    //     let p = [0, 1, 0, -1]
    //     seq {

    //     }


    printf "%A\n" (pattern 4 |> Seq.skip 3 |> Seq.take 50 |>  Seq.toList)








    0