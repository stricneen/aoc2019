module Day18

open Utils

type Location = { x:int; y:int; pos:char; dist:int }

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

    let rec printmap lst = 
        for x in 0 .. Array2D.length1 lst - 1 do
            let l = lst.[x,*] |> System.String
            printf "%A\n" l
   

    let prog = read2DArray "./data/day18a.txt"

    // #########
    // #b.A.@.a#
    // #########
    
    let getSurroundings locs =
        locs
        |> List.fold(fun acc loc -> 
            let n = { x=loc.x; y=(loc.y)-1; pos=prog.[loc.y-1, loc.x]; dist= loc.dist+1 }
            let e = { x=loc.x+1; y=loc.y; pos=prog.[loc.y, loc.x+1]; dist= loc.dist+1 }
            let s = { x=loc.x; y=loc.y+1; pos=prog.[loc.y+1, loc.x]; dist= loc.dist+1 }
            let w = { x=loc.x-1; y=loc.y; pos=prog.[loc.y, loc.x-1]; dist= loc.dist+1 }
            [n; e; s ;w] @ acc
        ) []
        |> List.where(fun x -> x.pos='.' || System.Char.IsLower x.pos)
        

    let availableKeys map = 
        let rec traverse points = 
            let x = getSurroundings points @ points
                    |> List.distinctBy(fun x -> x.x * 1000 + x.y)
            if List.length x = List.length points then
                x
            else 
                traverse x        
        let start = coordsOf map '@'
        let locations = traverse [ { x=fst start; y=snd start; pos='@'; dist=0 }]
        locations |> List.where(fun x -> System.Char.IsLower x.pos)

      
    let keys = availableKeys prog

    printf "%A\n\n" keys
    
    //printf "%A\n\n" prog

    printmap prog






    0