module Day4

open Utils

let day4 = 
    print "Advent of code - Day 4"

    let haspair number =
        let s  = number.ToString()
        s 
        |> Seq.windowed 2
        |> Seq.exists (fun x -> x.[0] = x.[1])


    let inc number =
        let s  = number.ToString()
        s 
        |> Seq.windowed 2
        |> Seq.forall (fun x -> x.[0] <= x.[1])


    let count = seq {367479 .. 893698}
                |> Seq.filter haspair
                |> Seq.filter inc
                |> Seq.length

    printn count

    print "Part 2"

    let count2 = seq {367479 .. 893698}
                |> Seq.filter haspair
                |> Seq.filter inc
                |> Seq.length

    printn count2
    // for n in 367479 .. 893698 do
    //     if haspair n && inc n then 
    //         printn n



    ()