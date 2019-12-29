module Day23

open Utils
open IntCode2


let day23 = 
    print "Advent of code - Day 23 - Category Six"

    let prog = readCSV "./data/day21.txt" 

    let comp = IntCode2 ""
    let inq = comp.Initialise prog
    
    let mutable finished = false

    comp.OutputReady.Add(fun output -> ())    

          
            


    while not finished do
        async { do! Async.Sleep(100) } |> ignore


    0