module Day18

open Utils

let day18 = 
    print "Advent of code - Day 18 - Many-Worlds Interpretation"

    let read2DArray path = 
        let input = readLines path
        let a2d = array2D input
        a2d
        
    let coordsOf ary chr =
        let rec loop (a: char[,]) c =
            let row = a.[c, *]
            let f = row |> Array.tryFindIndex (fun x -> x = chr)
            match f with 
            | None -> loop a (c + 1) 
            | Some x -> x,c

        loop ary 0

    let prog = read2DArray "./data/day18a.txt"

    let coords = coordsOf prog '@'
    printf "%A\n" prog


    printf "%A\n" (coordsOf prog '@')
    printf "%A\n" (coordsOf prog 'a')
    printf "%A\n" (coordsOf prog 'A')


    // let find2d arr chr = 
    //     for y in 0 .. Array2D.length2 do
    //         let row = arr.[*, y]
    //         let fnd = 


    // prog |> Array2D.


    // #########
    // #b.A.@.a#
    // #########





    0