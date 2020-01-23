module Day20

open System
open Utils
open IntCode2

type Location = { x:int; y:int; pos:char; dist:int;  }

type Node = { name:string; paths:list<int * string>; coords:int*int }

let day20 = 
    print "Advent of code - Day 20 - Donut Maze"

    //let prog = readCSV "./data/day20.txt" 
    
    let read2DArray path = 
        let input = readLines path |> Array.takeWhile (fun x -> x <> "*")
        let a2d = array2D input
        a2d
    
    let donut = read2DArray "./data/day20a.txt" 
    
    // printmap donut

    let getNodes = // AA, (3,4)   ZZ, (4,5) ....

        let getPairs x = x 
                         |> Array.windowed 3
                         |> Array.where(fun x -> Char.IsUpper x.[1] && ( Char.IsUpper x.[0] && x.[2] = '.' || Char.IsUpper x.[2] && x.[0] = '.'  ))
                         |> Array.map String.Concat

        let removeDots s = 
            s |> Seq.where(fun x -> x <> '.') |> String.Concat

        let rec rows c res = 
            if c > Array2D.length1 donut - 1 then
                res
            else
                let rowC = donut.[c, *]
                let row = getPairs rowC
                let rowCrds = row |> Array.map(fun x -> let offset = if x.[0] = '.' then 0 else 2
                                                        let ind = (rowC |> String).IndexOf x + offset
                                                        removeDots x, (ind, c))
                rows (c+1) (Array.append res rowCrds)

        let rec cols c res =
            if c > Array2D.length2 donut - 1 then
                res
            else
                let colC = donut.[*, c]
                let col = getPairs colC
                let colCrds = col |> Array.map(fun x -> let offset = if x.[0] = '.' then 0 else 2
                                                        let ind = (colC |> String).IndexOf x + offset
                                                        removeDots x, (c, ind ))
                cols (c+1) (Array.append res colCrds)

        Array.append (rows 0 [||]) (cols 0 [||])
    
    let nodes = getNodes 
    printf "%A\n" nodes


    let traverse nodes =
        let getSurroundings (map:char[,]) locs =
            let x = locs
                    |> List.fold(fun acc loc -> 
                        let n = { x=loc.x; y=(loc.y)-1; pos=map.[loc.y-1, loc.x]; dist= loc.dist+1 }
                        let e = { x=loc.x+1; y=loc.y; pos=map.[loc.y, loc.x+1]; dist= loc.dist+1 }
                        let s = { x=loc.x; y=loc.y+1; pos=map.[loc.y+1, loc.x]; dist= loc.dist+1 }
                        let w = { x=loc.x-1; y=loc.y; pos=map.[loc.y, loc.x-1]; dist= loc.dist+1 }
                        [n; e; s ;w] @ acc
                    ) []
                    |> List.where(fun x -> x.pos='.')
            x




        nodes
        |> Array.map(fun x ->
            {  name=fst x; paths= []; coords= snd x}
        )

    
    let graph = traverse nodes
    printf "%A\n" graph

    // let test = nodes
    //             |> Array.map(fun (_,(x,y)) ->
    //                 donut.[y,x]
    //             )

    // printf "%A\n" test
    0