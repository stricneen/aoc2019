module Day20

open System
open Utils
open IntCode2

type Location = { x:int; y:int; pos:string; direction:int; dist:int; visited: bool  }

type Bot = { location: string; depth: int; travelled: int; direction: int }

type Node = { name:string; paths:list<int * string>; visited: bool; }


let day20 = 
    print "Advent of code - Day 20 - Donut Maze"

    //let prog = readCSV "./data/day20.txt" 
    
    let read2DArray path = 
        let input = readLines path |> Array.takeWhile (fun x -> x <> "*")
        let a2d = array2D input
        a2d
    
    let donut = read2DArray "./data/day20.txt"
    
    // printmap donut

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
                                                        [{ x=ind; y=c; direction=0; pos=removeDots x; dist=0; visited=false}])
                rows (c+1) (Array.append res rowCrds)

        let rec cols c res =
            if c > Array2D.length2 donut - 1 then
                res
            else
                let colC = donut.[*, c]
                let col = getPairs colC
                let colCrds = col |> Array.map(fun x -> let offset = if x.[0] = '.' then 0 else 2
                                                        let ind = (colC |> String).IndexOf x + offset
                                                        [{ x=c; y=ind; direction=0; pos=removeDots x; dist=0; visited=false}])
                cols (c+1) (Array.append res colCrds)

        let r = rows 0 [||]
               
        let c = cols 0 [||]
               
        let outerX1 = ((r |> Array.minBy(fun x' -> (x' |> List.head).x)) |> List.head).x
        let outerX2 = ((r |> Array.maxBy(fun x' -> (x' |> List.head).x)) |> List.head).x
        let outerY1 = ((c |> Array.minBy(fun x' -> (x' |> List.head).y)) |> List.head).y
        let outerY2 = ((c |> Array.maxBy(fun x' -> (x' |> List.head).y)) |> List.head).y

        let r' = r  |> Array.map(fun x -> let x' = (x |> List.head) 
                                          let d = if x'.x = outerX1 || x'.x = outerX2 then -1 else 1
                                          [ { x' with direction = if x'.pos = "AA" || x'.pos = "ZZ" then 0 else d } ] )
                        
        let c' = c  |> Array.map(fun x -> let x' = (x |> List.head) 
                                          let d = if x'.y = outerY1 || x'.y = outerY2 then -1 else 1
                                          [ { x' with direction = if x'.pos = "AA" || x'.pos = "ZZ" then 0 else d } ] )

        let nodes = Array.append r' c' 
                    |> Array.toList 
        nodes 

    
    let coords = getNodes 
                 
    //printf "NODES : %A\n" coords

    let traverse nodes = // Get the vertices from each node 
        let getSurroundings (map:char[,]) locs =
            let x = locs
                    |> List.fold(fun acc loc -> 
                        let n = { x=loc.x; y=(loc.y)-1;direction=0;  pos=map.[loc.y-1, loc.x].ToString(); dist= loc.dist+1; visited=false }
                        let e = { x=loc.x+1; y=loc.y; direction=0; pos=map.[loc.y, loc.x+1].ToString(); dist= loc.dist+1; visited=false }
                        let s = { x=loc.x; y=loc.y+1; direction=0; pos=map.[loc.y+1, loc.x].ToString(); dist= loc.dist+1; visited=false }
                        let w = { x=loc.x-1; y=loc.y; direction=0; pos=map.[loc.y, loc.x-1].ToString(); dist= loc.dist+1; visited=false }
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
    
        let y' = y
                 |> List.map(fun (h::t) -> h, t)

        let y'' = y'
                  |> List.map(fun (h,t) -> h, (t |> List.map(fun x -> 
                        let mtch = y' |> List.find(fun m -> (fst m).x = x.x && (fst m).y = x.y)
                        { x with direction =  (fst mtch).direction }
                 )))
        
        y''
        

    let graph = traverse coords

    //printf "GRAPH  : %A\n" start

    let dijkstra graph = 

        let rec step g n = 

            let distanceTo s t =
                let can = (snd s) |> List.tryFind(fun x -> x.pos = t.pos)
                match can with
                | Some x  when (fst s).pos = "AA" -> x.dist + (fst s).dist
                | Some x -> x.dist + (fst s).dist + 1
                | None -> 0
                
            let eq x n = x.pos = (fst n).pos && x.x = (fst n).x && x.y = (fst n).y

            let ng = g |> List.map(fun x' ->
                let v = g |> List.find(fun (x,_) -> eq x n)
                let d =  distanceTo v (fst x')
                match x' with
                | (x,y) when  eq x n -> ({ x with visited=true} , y)  // current node
                | (x,y) when d > 0 && not x.visited -> ({ x with dist=if d < x.dist then d else x.dist } , y)
                | _ -> x'
            
            )
            //printf "STEP : %A\n\n\n" ng

            let remaining = ng |> List.where(fun (x,_) -> not x.visited)

            if List.isEmpty remaining then
                g
            else // Get next node
                let next = remaining
                              |> List.minBy(fun (x,_) -> x.dist)
                step ng next



        // Set distances to something big
        let start = graph |> List.map(fun x -> 
            match x with
            | (x,y) when x.pos="AA" -> x,y
            | (x,y) -> ({ x with dist=1000000} , y)
        )

        let aa = graph |> List.find(fun (x,_) -> x.pos = "AA" )
        step start aa
   
    // let shortest = dijkstra graph
    //                 |> List.find(fun (x,_) -> x.pos = "ZZ" )
    // printf "Part 1 : %A\n" (fst shortest).dist

    let dijkstra3D (graph:list<Location * list<Location>>) = 

        // printf "Graph : %A\n\n\n" graph 
        let exit x = x.pos = "ZZZZ"

        let fltr moves = 
            moves
            |> List.distinct 
            |> List.filter(fun x -> x.depth >= 0)
            |> List.groupBy(fun x -> x.location + x.depth.ToString() + x.direction.ToString()) 
            |> List.map(fun (_,y) -> (y |> List.minBy(fun x -> x.travelled)))
                   

        let rec step bots = 
           
            printf " Bots : %A\n" (List.length bots)
                        
            //Console.ReadKey() |> ignore
            let move = bots
                       |> List.fold(fun a e -> 
                            let paths = graph |> List.tryFind(fun (x,_) -> x.pos = e.location && x.direction = e.direction * -1 )
                            match paths with
                            | Some x -> let moves = (snd x)
                                                        |> List.map(fun x -> { 
                                                            location = x.pos; 
                                                            depth = e.depth + x.direction; 
                                                            travelled = e.travelled + x.dist + 1; 
                                                            direction = if exit x then 0 else x.direction }
                                                        )
                                        a @ moves 
                            | None -> a
                       ) []

            let exit = move |> List.tryFind(fun x -> x.location = "ZZ" && x.depth = 0)
            match exit with 
            | Some x -> exit
            | None -> step (fltr move)

        let bot = [ { location = "AA"; depth = 0; travelled = 0; direction = 0; } ]
        step bot

    let shortest = dijkstra3D graph 
    printf "SHORTEST : %A\n"  { shortest.Value with travelled = shortest.Value.travelled - 1 }


    0
