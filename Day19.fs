module Day19

open Utils
open IntCode3

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


    let mutable count = 0
    for y in 0L .. 49L do
        for x in 0L .. 49L do
            let (x,y), drone = isInField x y
            if drone = 1L then
                count <- count + 1
            printAt (int x) (int y) (if drone = 0L then "." else "#")     
    print "Part 1"
    pl count      

    let hasOppositeCorner x y s =
        let delta = s - 1L
        inField (x-delta) (y+delta)
        
    let start x y s =

        let rec loop x y =
            if not (inField x y) then
               // printf "Not in field %A %A\n" x y
                loop x (y+1L)
            else
                if hasOppositeCorner x y s then
                    printf "Found %A @ %A,%A\n" s x y
                    s
                else 
                    loop (x+1L) y
        loop x y

    start 10L 10L 1L |> ignore
    start 10L 10L 2L |> ignore
    start 10L 10L 3L |> ignore
    start 10L 10L 4L |> ignore
    start 10L 10L 5L |> ignore

    while not finished do
        async { do! Async.Sleep(100) } |> ignore


    0