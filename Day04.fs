module Day4

open Utils

// Passwords

let day4 = 
    print "Advent of code - Day 4 - Secure Container"

    let haspair number =
        number.ToString() 
        |> Seq.windowed 2
        |> Seq.exists (fun x -> x.[0] = x.[1])

    let inc number =
        number.ToString()
        |> Seq.windowed 2
        |> Seq.forall (fun x -> x.[0] <= x.[1])

    let singlepair number =
        "x" + number.ToString() + "x"
        |> Seq.windowed 4
        |> Seq.exists (fun x -> x.[0] <> x.[1] && x.[1] = x.[2] && x.[2] <> x.[3] )

    let valid = seq {367479 .. 893698}
                |> Seq.filter haspair
                |> Seq.filter inc
                
    printn (valid |> Seq.length)

    print "Part 2"

    let valid2 = valid
                 |> Seq.filter singlepair

    printn (valid2 |> Seq.length)




    ()