module Day20

open System
open Utils
open IntCode2

type Location = { x:int; y:int; pos:string; dist:int; visited: bool  }

// type Node = { name:string; paths:list<int * string>; visited: bool; }


let day20 = 
    print "Advent of code - Day 20 - Donut Maze"

    //let prog = readCSV "./data/day20.txt" 
    
    let read2DArray path = 
        let input = readLines path |> Array.takeWhile (fun x -> x <> "*")
        let a2d = array2D input
        a2d
    
    let donut = read2DArray "./data/day20a.txt"
    
    printmap donut

    let getNodes = // Get list of nodes from the input

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
                                                        [{ x=ind; y=c; pos=removeDots x; dist=0; visited=false}])
                rows (c+1) (Array.append res rowCrds)

        let rec cols c res =
            if c > Array2D.length2 donut - 1 then
                res
            else
                let colC = donut.[*, c]
                let col = getPairs colC
                let colCrds = col |> Array.map(fun x -> let offset = if x.[0] = '.' then 0 else 2
                                                        let ind = (colC |> String).IndexOf x + offset
                                                        [{ x=c; y=ind; pos=removeDots x; dist=0; visited=false}])
                cols (c+1) (Array.append res colCrds)

        (Array.append (rows 0 [||]) (cols 0 [||])) |> Array.toList
    
    let coords = getNodes 
    //printf "NODES : %A\n" nodes

    let traverse nodes = // Get the vertices from each node 
        let getSurroundings (map:char[,]) locs =
            let x = locs
                    |> List.fold(fun acc loc -> 
                        let n = { x=loc.x; y=(loc.y)-1; pos=map.[loc.y-1, loc.x].ToString(); dist= loc.dist+1; visited=false }
                        let e = { x=loc.x+1; y=loc.y; pos=map.[loc.y, loc.x+1].ToString(); dist= loc.dist+1; visited=false }
                        let s = { x=loc.x; y=loc.y+1; pos=map.[loc.y+1, loc.x].ToString(); dist= loc.dist+1; visited=false }
                        let w = { x=loc.x-1; y=loc.y; pos=map.[loc.y, loc.x-1].ToString(); dist= loc.dist+1; visited=false }
                        [n; e; s ;w] @ acc
                    ) []
                    |> List.where(fun x -> x.pos=".")
            x

        let rec traverse' (points: Location list) c = 
            let ends = points |> List.where(fun x -> x.dist = c)
            let x = getSurroundings donut ends @ points
                    |> List.sortBy(fun x -> x.dist)
                    |> List.distinctBy(fun x -> x.x * 1000 + x.y)
            if List.length x = List.length points then
                x
            else 
                traverse' x (c+1)

        let raw = nodes |> List.map(fun x -> traverse' x 0)

        let flatnodes = nodes |> List.concat
        
        let y = raw
                |> List.map(fun x -> 
                    // printf "%A\n" x
                    x |> List.map(fun x' -> 

                        let e = flatnodes 
                                |> List.tryFind(fun n -> n.x = x'.x && n.y = x'.y)

                        match e with
                        | None -> x'
                        | Some n -> { x' with pos = n.pos }
                    )
                    |> List.where(fun x -> x.pos <> ".")
                )
                
        y

    let graph = traverse coords
    printf "GRAPH  : %A\n" graph

    let findNode pos = graph |> List.find(fun x -> (x |> List.head).pos = pos)


   // let nodes = 

    // printf "%A\n" start
    // printf "%A\n" finish

    // let nodes = 
    //     graph
    //     |> List.map(fun x -> { name=x.name; paths=x.paths; visited=false;  })

    
    
       
//  { name:string;      paths:list<int * string>;   visited: bool; }
    
    // let shortest = dijkstra graph
    // printf "Path : %A\n" shortest

    0