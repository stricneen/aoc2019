module Day18

open Utils
open System

type Location = { x:int; y:int; pos:char; dist:int; doors: char list }

type State = { total:int; map: char[,] }

type Key = { key: char; dist: int; doors: char list }

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
        if System.Char.IsUpper current then
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
    printmap prog

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

        // Calculate the next iteration bfs

        // [|0;0;0;0;1;0;0;0;1;0;0|]
        

        let rec nextIteration iters doors =
            let nextIters = Array.copy iters

            let sortedDoors = doors
                              |> List.sortBy(fun x -> (List.length x.doors), x.key)

            let accessible = sortedDoors 
                             |> List.where(fun x -> List.isEmpty x.doors)

            let index = List.length doors
            let last = iters.[index]
            
            printf "doors : %A\n" sortedDoors
            printf "index : %A\n" index
            if index = 0 then   
                Console.ReadKey() |> ignore
                ()
            printf "iters : %A\n" nextIters
           // printf "acces : %A\n" accessible
            
            let keyToGet = accessible |> List.tryItem last
            //printf "acces : %A\n" accessible
            //printf "going : %A\n" doors |> List.where(fun x -> x = doors.[iters.[index]])
                               
            // let keyToGet = accessible |> List.tryFind last

            let r = match keyToGet with
                        | None -> Array.set nextIters index 0
                                  nextIters
                        | Some key -> let remaining = sortedDoors
                                                      |> List.where(fun x -> x <> key)
                                      Console.ReadKey()

                                      nextIteration nextIters remaining
            r


    //         if (List.length accessible) = last then
    //                 //List.length accessible 
    //                 //List.isEmpty accessible then
    //             printf "to go : %A\n" accessible

    //             Array.set nextIters index (last + 1)
    //             nextIters
    //             // finished
    //         else
    //             Array.set nextIters index 0
    //    //         printf "carry : %A\n" (List.length accessible)

    //             let remaining = sortedDoors
    //                             |> List.where(fun x -> x <> sortedDoors.[last])
    //             printf "remaining : %A\n\n\n\n" remaining
    //            // Console.ReadKey()
    //             nextIteration nextIters remaining
                

        let split doors = 

            let accessible = doors |> List.where(fun x -> List.isEmpty x.doors)
            let index = iters.[List.length doors]
            //printn index
            //printf "Going  : %A\n" accessible.[index]
            //printf "%A\n" accessible
            //printf "h : %A\n\n\n\n" accessible.[index]
            //printf "t : %A\n\n\n\n" (doors |> List.where(fun x -> x <> accessible.[index]))
             
            accessible.[index], doors 
                                |> List.where(fun x -> x <> accessible.[index])
                                |> List.sortBy(fun x -> (List.length x.doors), x.key)
                             

//            if List.length accessible  index then 

        let move doors =
            let h, t = split doors  // search here
            printf "removing key : %A\n" h.key
            (t |> List.map(fun x ->
            {
                key = x.key
                dist = keyToKey h.key x.key + steps
                doors = x.doors |> List.where(fun x -> Char.ToLower x <> h.key)
            }), h.dist)

        let rec step keys steps = 
            //printf "Step : %A\n" steps
            //printf "Iter : %A\n" nextIters
            //printf "[%A]  %A\n\n" (List.length keys) keys

            // Check for dead end
            
            let doors = keys 
                        |> List.sortBy(fun x -> (x.key, List.length x.doors))

           // printf "KEYS : %A\n" doors

            if List.isEmpty keys then
                steps, nextIteration iters (snd (dist |> List.find(fun x-> fst x = '@')))
            else
                let s, dist = move doors 
                step s (dist + steps)

        step keys 0 




    //let iters = [|0; 0; 0; 0; 0; 0; 0; 0; 0; 4|]
    let iters = [|0; 0; 0; 0; 0; 0; 1; 2; 3; 4|]
    let steps, i = path dists iters

// ########################
// #@..............ac.GI.b#
// ###d#e#f################
// ###A#B#C################
// ###g#h#i################
// ########################
// *
// 81
// a, c, f, i, d, g, b, e, h
    0

    let mutable iters = Array.init (List.length dists) (fun x -> 0)
    let mutable min = 1000000000
    let mutable run = true
    while run do

        let steps, i = path dists iters
        iters <- i
//        let iters = [|0;0;0;0;0;0;0;0;0;0;5|]
        printf "Next : %A\n" i
        printf "Stps : %A\n" steps
        printf "Min : %A\n\n\n" min

      //  Console.ReadKey()

        if steps < min then
            min <- steps

        if iters.[0] > Array.length iters then
            //printf "Min : %A\n" min
            run <- false

    0
