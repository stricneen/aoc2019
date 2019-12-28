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
        let start = coordsOf prog '@'
        let s = [ { x=fst start; y=snd start; pos='@'; dist=0 }]
        printf "%A\n\n" s
        let x = getSurroundings s
        x


    let keys = availableKeys prog

    printf "%A\n\n" keys
    // prog |> Array2D.







    0