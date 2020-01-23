module Day20

open System
open Utils
open IntCode2

let day20 = 
    print "Advent of code - Day 20 - Donut Maze"

    //let prog = readCSV "./data/day20.txt" 
    
    let read2DArray path = 
        let input = readLines path |> Array.takeWhile (fun x -> x <> "*")
        let a2d = array2D input
        a2d
    
    let donut = read2DArray "./data/day20a.txt" 
    
    printmap donut

    let getNodes =
        let rec loop r res =
            if r > Array2D.length1 donut - 1 then
                res
            else
                let row = donut.[r, *]

                let ltrs = row |> Array.fold(fun a c -> 
                    if Char.IsUpper c then
                        a
                    else
                        a
                ) []

               // let f = row |> Array.tryFindIndex (fun x -> x = chr)
                
                let t = "XX"
                // match f with 
                // | None -> loop a (c + 1) 
                // | Some x -> x,c

                loop (r+1) (t :: res)
        loop 0 []


    printf "%A\n" getNodes


    // let mutable finished = false
    // while not finished do
    //     async {
    //         do! Async.Sleep(100) } |> ignore


    0