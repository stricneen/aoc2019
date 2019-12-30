module Day23

open Utils
open IntCode2


let day23 = 
    print "Advent of code - Day 23 - Category Six"

    let prog = readCSV "./data/day23.txt" 

    let outputNic x = 
        printf "%A'n" x

    // let network = List.init 50 (fun x -> 
    //     let c = IntCode2 (x.ToString())
    //     let q = c.Initialise prog
    //     q.Post (int64 x)
    //     c.OutputReady.Add(outputNic)
    //     (c,q)
    // )
    // printf "%A'n" network

    let comp = IntCode2 "nic-0"
    let inq = comp.Initialise prog
    
    comp.OutputReady.Add(fun x -> printf "%A\n" x)
    inq.Post 0L

    let mutable finished = false

//    comp.OutputReady.Add(fun output -> ())    

          
            


    while not finished do
        async { do! Async.Sleep(100) } |> ignore


    0