module Day18

open Utils
open System

type Location = { x:int; y:int; pos:char; dist:int; doors: char list }

type State = { total:int; map: char[,] }

type Key = { key: char; dist: int; doors: char list }

type Path = { at: string; visited: string; travelled: int; remaining: Key list }

let day18 = 
    print "Advent of code - Day 18 - Many-Worlds Interpretation"

    let read2DArray path = 
        let input = readLines path |> Array.takeWhile (fun x -> x <> "*")
        let a2d = array2D input
        a2d
        
    let coordsOf ary chr =
        let rec loop (a: char[,]) c =
            if c > Array2D.length1 ary - 1 then
                -1,-1
            else
                let row = a.[c, *]
                let f = row |> Array.tryFindIndex (fun x -> x = chr)
                match f with 
                | None -> loop a (c + 1) 
                | Some x -> x,c
        loop ary 0

    let rec printmap lst = 
        for x in 0 .. Array2D.length1 lst - 1 do
            let l = lst.[x,*] |> String
            printf "%A\n" l
   

    

    // #########
    // #b.A.@.a#
    // #########
    
    let doors visitied current = 
        if System.Char.IsUpper current then // || System.Char.IsLower current then //  || System.Char.IsLower current then
            visitied @ [current]
        else
            visitied

    let getSurroundings (map:char[,]) locs =
        let x = locs
                |> List.fold(fun acc loc -> 
                    let n = { x=loc.x; y=(loc.y)-1; pos=map.[loc.y-1, loc.x]; dist= loc.dist+1; doors= doors loc.doors map.[loc.y-1, loc.x] }
                    let e = { x=loc.x+1; y=loc.y; pos=map.[loc.y, loc.x+1]; dist= loc.dist+1; doors= doors loc.doors map.[loc.y, loc.x+1] }
                    let s = { x=loc.x; y=loc.y+1; pos=map.[loc.y+1, loc.x]; dist= loc.dist+1; doors= doors loc.doors map.[loc.y+1, loc.x] }
                    let w = { x=loc.x-1; y=loc.y; pos=map.[loc.y, loc.x-1]; dist= loc.dist+1; doors= doors loc.doors map.[loc.y, loc.x-1] }
                    [n; e; s ;w] @ acc
                ) []
                |> List.where(fun x -> x.pos='.' || x.pos='@' || System.Char.IsLower x.pos || System.Char.IsUpper x.pos)
       // printf "acc: %A\n" x
        //System.Console.ReadKey() x
        x
                
    let availableKeys map from = 
        let rec traverse (points: Location list) c = 
            let ends = points |> List.where(fun x -> x.dist = c)
            let x = getSurroundings map ends @ points
                    |> List.sortBy(fun x -> x.dist)
                    |> List.distinctBy(fun x -> x.x * 1000 + x.y)
            if List.length x = List.length points then
                x
            else 
                traverse x (c+1)

        let start = coordsOf map from
        let locations = traverse [ { x=fst start; y=snd start; pos=from; dist=0; doors= [] }] 0
        locations |> List.where(fun x -> System.Char.IsLower x.pos)

    let printState s = 
        s |> List.iter(fun x -> printf "Total : %A\n" x.total
                                printmap x.map
        )

    let prog = read2DArray "./data/day18.txt"
    // printmap prog

    //let startState = { total=0; map=prog }
    // let t = loop [ startState ]
   
    // Get all keys and coords
    let getKeys map =   
        [ 'a' .. 'z' ] @ [ '@' ]
        |> List.map (fun x -> x, coordsOf map x)
        |> List.filter (fun (_,c) -> snd c > -1 )

    let locsToKeys locs = 
        locs 
        |> List.map(fun x -> { key = x.pos; dist = x.dist; doors = x.doors |> List.map Char.ToLower })
        |> List.sortBy (fun x -> List.length x.doors)

    let distances map keys =
        keys 
        |> List.map(fun (key, (x,y)) -> (key, (x,y), availableKeys map key )
                >> (fun (key, _, locs) -> key, locsToKeys locs))      

    let keys = getKeys prog
    let dists = distances prog keys

    //printf "%A\n" dists
    let _, keys = dists |> List.find(fun x-> fst x = '@')

    // Get the distance between two keys
    let keyToKey k1 k2 =
        let k1' = dists
                  |> List.find(fun (x, _) -> x = k1)   // 'char * Key list' 
        let k2' = (snd k1')
                  |> List.find(fun x -> x.key = k2)
        k2'.dist

    let visited = keys |> List.where(fun x -> List.isEmpty x.doors)
                       |> List.sortBy(fun x -> x.key)
    
    // make the first moves

    let first = visited
               |> List.map(fun x -> { 
                    at = x.key.ToString();
                    visited = x.key.ToString(); 
                    travelled = x.dist; 
                    remaining = (keys 
                                |> List.where(fun x' -> x'.key <> x.key))  // Remove just visited
                                |> List.map(fun x' -> { x' with doors = x'.doors |> List.where(fun x'' -> x'' <> x.key); dist = keyToKey x.key x'.key  }) }) // Remove door
    
    
    //printf "%A\n" first 

    let traverse (start: Path list) = 

        let rec move from =
            
            let s = from |> List.fold(fun a c ->

                    let accessible = c.remaining
                                       |> List.where(fun x -> List.isEmpty x.doors)
                                       |> List.sortBy(fun x -> x.key)

                    let moves =
                        accessible |> List.map(fun x -> { 
                        at = x.key.ToString();
                        visited = c.visited + x.key.ToString(); 
                        travelled = x.dist + c.travelled; 
                        remaining = (c.remaining 
                                    |> List.where(fun x' -> x'.key <> x.key))  // Remove just visited
                                    |> List.map(fun x' -> { x' with doors = x'.doors |> List.where(fun x'' -> x'' <> x.key); dist = keyToKey x.key x'.key  }) }) // Remove door
        
                    let a' = List.append moves a
                    a'        
                    ) []

        //    printf "%A\n" s
          
            
            let shortest = 
                s |> List.map(fun x -> 
                    let hash = x.visited.Substring(0, x.visited.Length - 1) |> Seq.sort |> String.Concat
                    hash, x)
                  |> List.groupBy(fun x -> fst x, (snd x).at)
                  |> List.map((fun (_,x) -> x |> List.minBy(fun (_,y) -> y.travelled))
                           >> (fun (_,x) -> x))

            printn (List.length shortest)
            if (shortest |> List.forall(fun x -> List.isEmpty x.remaining)) then
                shortest
            else 
                move shortest



        move start


    let x = traverse first
    let min = x |> List.minBy(fun x -> x.travelled)
    printf "%A\n" min

    0
