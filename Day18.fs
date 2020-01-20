module Day18

open Utils
open System

type Location = { x:int; y:int; pos:char; dist:int; doors: char list }

type State = { total:int; map: char[,] }

type Key = { key: char; dist: int; doors: char list }

let day18 = 
    //print "Advent of code - Day 18 - Many-Worlds Interpretation"

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
        locations |> List.where(fun x -> System.Char.IsLower x.pos)// || Char.IsUpper x.pos)

    let printState s = 
        s |> List.iter(fun x -> printf "Total : %A\n" x.total
                                printmap x.map
        )

    let prog = read2DArray "./data/day18a.txt"
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
        |> List.map(fun x -> { key = x.pos; dist = x.dist; doors = x.doors })
        |> List.sortBy (fun x -> List.length x.doors)

    let distances map keys =
        keys 
        |> List.map(fun (key, (x,y)) -> (key, (x,y), availableKeys map key )
                >> (fun (key, _, locs) -> key, locsToKeys locs))      

    let keys = getKeys prog
    let dists = distances prog keys








    let path dist (iters: int array) =
                

       // let nextIters = Array.copy iters
        let _, keys = dist |> List.find(fun x-> fst x = '@')
        
        //let bfs = Array.init (List.length keys) (fun _ -> 0)
        let steps = 0

        // Get the distance between two keys
        let keyToKey k1 k2 =
            let k1' = dists
                      |> List.find(fun (x, _) -> x = k1)   // 'char * Key list' 
            let k2' = (snd k1')
                      |> List.find(fun x -> x.key = k2)
            k2'.dist

        let mutable nextIter: option<int array> = None

        let rec zeroTo array i =
            Array.set array i 0
            if i + 1  < (Array.length array) then   
                zeroTo array (i + 1)
         
        let rec nextIteration iters doors =
            iters
                

        let getKey doors = 

            let accessible = doors 
                             |> List.where(fun x -> List.isEmpty x.doors)
                             |> List.sortBy(fun x -> x.key)

            let index = iters.[List.length doors]

                
           // if iters.[9] = 0 && iters.[8] = 0 && iters.[7] = 2 && iters.[6] >= 2 then
           // printf "Doors : %A\n" doors
            // printn index
            // printf "%A\n" iters
            // printf "Acc : %A\n" accessible
            // printf "Going  : %A\n" accessible.[index]
            // printf "Remainder : %A\n\n\n\n" (doors |> List.where(fun x -> x <> accessible.[index]))
            // Console.ReadKey()
            // ()
            

            if List.length accessible <= index then // gone too high
                // inform next route
                // let cpy = Array.copy iters
                // zeroTo cpy (List.length doors) 
                // Array.set cpy (List.length doors-1) (cpy.[List.length doors-1]+1)
                printn index
                nextIter <- Some iters
                accessible.[0], []  // dirty
            else
                accessible.[index], doors 
                                |> List.where(fun x -> x <> accessible.[index])
                                
                             

        let move doors =
            let keyCollected, remaining = getKey doors  // search here
           // printf "removing key : %A\n" keyCollected.key
            (remaining |> List.map(fun x ->
            {
                key = x.key
                dist = keyToKey keyCollected.key x.key + steps
                doors = x.doors |> List.where(fun x -> Char.ToLower x <> keyCollected.key)
            }), keyCollected)
            

        let rec step keys steps order= 
            if List.isEmpty keys then // Route complete
                steps, nextIter, order
            else
                let keys', key = move keys 
                step keys' (key.dist + steps) (key.key :: order)

        step keys 0 []




    let min = [|0; 0; 0; 0; 1; 0; 3; 2; 0; 0|]

    let generateIters n = 
        let iter = Array.init n (fun x -> 0)

        let rec inc index = 
          //  printn index
            if iter.[index] < index then
               // printn index
                Array.set iter index (iter.[index] + 1)
                ()
            else
                Array.set iter index 0
                inc (index - 1)

        seq {
            while iter.[0] = 0 do
                yield iter
                inc (n-1)
        }


    // for x in (generateIters 10) do
    //     printf "%A\n"
    //     let x' = x 
    //     ()
        
           // let iters = [|0; 0; 0; 0; 0; 0; 1; 2; 3; 4|]
    // let steps, i, order = path dists iters
    // printf "%A\n" iters
    //printf "Dist : %A\n" steps
    // printf "i : %A\n" i
    


// ########################
// #@..............ac.GI.b#
// ###d#e#f################
// ###A#B#C################
// ###g#h#i################
// ########################
// *
// 81
// a, c, f, i, d, g, b, e, h


    
    
    let keys = List.length dists
    let mutable iters = Array.init keys (fun x -> 0)
    print "Starting ..."
    let mutable min = 1000000000
    for iters in (generateIters keys) do
//    iters <-  [|0; 0; 0; 0; 0; 0; 2; 2; 2; 2|]

        //let mutable run = true

        //printf "I : %A\n" iters
        //Console.ReadKey() |> ignore

        let steps, i, order = path dists iters

        // let sorted = String.Join ("",(order |> List.rev))
      
        //              // acfidgbeh
        // if sorted.StartsWith("acfi") then
        //     printf "Next : %A\n" iters
        //     printf "Order : %A\n" sorted
        //     printf "Stps : %A\n" steps
        //     printf "Min : %A\n\n\n" min
        if i.IsSome then
            printf "%A\n" i.Value

        if steps < min && i.IsNone then
            min <- steps
            printf "Itr : %A\n" iters
            printf "Min : %A\n\n" min
     
//        if iters.[0] > Array.length iters then
    

    0
