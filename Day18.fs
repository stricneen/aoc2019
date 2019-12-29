module Day18

open Utils

type Location = { x:int; y:int; pos:char; dist:int }

type State = { total:int; map: char[,] }

let day18 = 
    print "Advent of code - Day 18 - Many-Worlds Interpretation"

    let read2DArray path = 
        let input = readLines path
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
            let l = lst.[x,*] |> System.String
            printf "%A\n" l
   

    

    // #########
    // #b.A.@.a#
    // #########
    
    let getSurroundings (map:char[,]) locs =
        let x = locs
                |> List.fold(fun acc loc -> 
                    let n = { x=loc.x; y=(loc.y)-1; pos=map.[loc.y-1, loc.x]; dist= loc.dist+1 }
                    let e = { x=loc.x+1; y=loc.y; pos=map.[loc.y, loc.x+1]; dist= loc.dist+1 }
                    let s = { x=loc.x; y=loc.y+1; pos=map.[loc.y+1, loc.x]; dist= loc.dist+1 }
                    let w = { x=loc.x-1; y=loc.y; pos=map.[loc.y, loc.x-1]; dist= loc.dist+1 }
                    [n; e; s ;w] @ acc
                ) []
                |> List.where(fun x -> x.pos='.' || System.Char.IsLower x.pos)
       // printf "acc: %A\n" x
        //System.Console.ReadKey()
        x
                
        

    let availableKeys map= 
        let rec traverse points c = 
            let ends = points |> List.where(fun x -> x.dist = c)
            let x = getSurroundings map ends @ points
                    |> List.sortBy(fun x -> x.dist)
                    |> List.distinctBy(fun x -> x.x * 1000 + x.y)
            //printf "traverse: %A\n" x
            //System.Console.ReadKey()
            if List.length x = List.length points then
                x
            else 
                traverse x (c+1)
        let start = coordsOf map '@'
        //printf "start: %A\n" start
        let locations = traverse [ { x=fst start; y=snd start; pos='@'; dist=0 }] 0
        locations |> List.where(fun x -> System.Char.IsLower x.pos)

    let printState s = 
        s |> List.iter(fun x -> printf "Total : %A\n" x.total
                                printmap x.map
        )

    let useKey map pos = 
        let cx, cy = coordsOf map '@'
        let dx, dy = coordsOf map (System.Char.ToUpper pos.pos)
        let cpy = Array2D.copy map
        Array2D.set cpy pos.y pos.x '@'
        Array2D.set cpy cy cx '.'
        if dy > -1 then Array2D.set cpy dy dx '.'
        cpy
    
    let prog = read2DArray "./data/day18.txt"
    printmap prog

    let rec loop states = 
        let keys = states
                   |> List.map(fun x -> x, availableKeys x.map)
        let newStates = keys
                        |> List.fold(fun a x -> 
                                let state, keys = x
                                let unlock = keys
                                           |> List.map(fun x -> {total=state.total + x.dist; map=useKey state.map x})
                                unlock @ a
                        ) []
                        // |> List.sortBy(fun x -> x.total)
                        // |> List.truncate 10000
        if List.isEmpty newStates then
            states
        else
            //print "\n\n"
            printn (List.length newStates)
            
            // printState states
            //System.Console.ReadKey()
            
            loop newStates
    

    let startState = { total=0; map=prog }
    let t = loop [ startState ]
   

    let min = t |> List.minBy (fun x -> x.total)
    
    print "DONE"
    printState [min]


    // let keys = availableKeys prog
    // let k = keys |> List.head
    // let t = useKey prog k
    // printf "%A\n\n" keys
    // printmap t


    // let keys' = availableKeys t
    // printf "%A\n\n" keys'
    // let k' = keys' |> List.head
    // let t' = useKey t k'
    // printmap t'






    0