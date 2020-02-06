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

        
    let mutable count = 0
    for x in 0L .. 49L do
        for y in 0L .. 49L do
            let (x,y), drone = isInField x y
            if drone = 1L then
                count <- count + 1
            //printAt (int y) (int x) (if drone = 0L then "." else "#")     
    print "Part 1"
    pl count      


    while not finished do
        async { do! Async.Sleep(100) } |> ignore


    0