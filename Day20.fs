module Day20

open System
open Utils
open IntCode2

let day20 = 
    //print "Advent of code - Day 20 - Donut Maze"

    //let prog = readCSV "./data/day20.txt" 
    
    let read2DArray path = 
        let input = readLines path |> Array.takeWhile (fun x -> x <> "*")
        let a2d = array2D input
        a2d
    
    let donut = read2DArray "./data/day20a.txt" 
    
    printmap donut

    let getNodes = // AA, (3,4)   ZZ, (4,5) ....

        let getPairs x = x 
                         |> Array.windowed 3
                         |> Array.where(fun x -> Char.IsUpper x.[1] && ( Char.IsUpper x.[0] && x.[2] = '.' || Char.IsUpper x.[2] && x.[0] = '.'  ))
                         |> Array.map String.Concat

        let rec loop c res =
            if c > Array2D.length1 donut - 1 then
                res
            else
                let rowC = donut.[c, *]
                let colC = donut.[*, c]
                
                let row = getPairs rowC
                let col = getPairs colC

                let rowCrds = row |> Array.map(fun x -> let offset = if x.[0] = '.' then 0 else 3
                                                        let ind = (rowC |> String).IndexOf x + offset
                                                        x, (ind, c))
                let colCrds = col |> Array.map(fun x -> let offset = if x.[0] = '.' then 0 else 3
                                                        let ind = (colC |> String).IndexOf x + offset
                                                        x, (c, ind ))
                
                let rc = Array.append rowCrds colCrds
                loop (c+1) (Array.append res rc)


        loop 0 [||]
        





    printf "%A\n" getNodes


    // let mutable finished = false
    // while not finished do
    //     async {
    //         do! Async.Sleep(100) } |> ignore


    0