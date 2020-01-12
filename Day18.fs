module Day18

open Utils
open System

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
            let l = lst.[x,*] |> String
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
                |> List.where(fun x -> x.pos='.' || x.pos='@' || System.Char.IsLower x.pos || System.Char.IsUpper x.pos)
       // printf "acc: %A\n" x
        //System.Console.ReadKey() x
        x
                

    let availableKeys map from = 
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
        let start = coordsOf map from
        //printf "start: %A\n" start
        let locations = traverse [ { x=fst start; y=snd start; pos=from; dist=0 }] 0
        locations |> List.where(fun x -> System.Char.IsLower x.pos || Char.IsUpper x.pos)

    let printState s = 
        s |> List.iter(fun x -> printf "Total : %A\n" x.total
                                printmap x.map
        )

    
    let getDistance map start finish =
        ()

    let getDistances map key =
        ()


    let prog = read2DArray "./data/day18a.txt"
    printmap prog

    //let startState = { total=0; map=prog }
    // let t = loop [ startState ]
   
    // Get all keys and coords
    let getKeys map = 
        [ 'a' .. 'z' ]
        |> List.map (fun x -> x, coordsOf map x)
        |> List.filter (fun (_,c) -> snd c > -1 )

    // Get distance to every other key
    let distances map keys =
        keys 
        |> List.map(fun (key, (x,y)) ->
            (key, (x,y), availableKeys map key )
        )

    let keys = getKeys prog
    let dist = distances prog keys

   // let x = availableKeys prog '@'

    printf "%A\n" dist
   // Get a list of keys on grid
 
 
// want 
//  @  [ b: 23  c : 33 ]   ABC

//  a x,y   [ b: 23: [ABV]  c : 33: [ERF] ]  
//  b
//  c





    0
