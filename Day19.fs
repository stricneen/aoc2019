module Day19

open Utils
open IntCode3

// 9761057 too hight
// 8771057

let day19 = 
    print "Advent of code - Day 19 - Tractor Beam"

    let mutable counter = 0L
    let mutable finished = false
    let prog = readCSV "./data/day19.txt" 
    
    let isInField x y =
        let comp = IntCode3("")
        let inq = comp.Initialise (prog |> Array.copy)
        let aut = new System.Threading.AutoResetEvent(false);
        inq.Post x
        inq.Post y
        let mutable out = 0L
        comp.OutputReady.Add(fun output ->
            if output <> -99999L then 
                aut.Set() |> ignore
                out <- output
        )    
        aut.WaitOne() |> ignore
        (x,y), out

    let inField x y =
        let (x', y'), o = isInField x y
        o = 1L


    // let mutable count = 0
    // for y in 0L .. 49L do
    //     for x in 0L .. 49L do
    //         let (x,y), drone = isInField x y
    //         if drone = 1L then
    //             count <- count + 1
    //         printAt (int x) (int y) (if drone = 0L then "." else "#")     
    // print "Part 1"
    // pl count      

    let hasOppositeCorner x y s =
        let delta = s - 1L
        inField (x-delta) (y+delta)
        
    let start x y =
       
        let rec loop x y s =
            if not (inField x y) then
               // printf "Not in field %A %A\n" x y
                loop x (y+1L) s
            else
                if hasOppositeCorner x y s then
                    printf "Found %A @ %A,%A\n" s (x-s+1L) y
                    loop x y (s+1L)
                else 
                    loop (x+1L) y s
        loop 10L 10L 1L
    
    start 10L 10L 

    // let loop100 =
    //     let rec inner c =
    //         start 10L 10L c |> ignore
    //         inner (c+1L)
    //     inner 1L

    // loop100

    while not finished do
        async { do! Async.Sleep(100) } |> ignore


    0