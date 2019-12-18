module Day11

open Utils
open IntCode2

let day11 = 
    print "Advent of code - Day 11 - Space Police"

    let prog = readCSV "./data/day11.txt" 

    let comp = IntCode2("painter")
    let inq = comp.Initialise prog
    
    
    let output v =
        printf "%A\n" v

    comp.OutputReady.Add(output)

    inq.Post(0L); //Start on black
    
    
    
    
    let mutable finished = false
    comp.OutputReady.Add(fun o -> finished <- o = -99999L)
    while not finished do
        async { do! Async.Sleep(10)
        
        
        
        
        
        
         } |> ignore


    // System.Console.ReadKey() |> ignore
    // System.Console.ReadKey() |> ignore
   
    0