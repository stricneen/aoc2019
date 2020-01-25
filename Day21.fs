module Day21

open Utils
open IntCode2

let day21 = 
    print "Advent of code - Day 21 - Springdroid Adventure"

    let prog = readCSV "./data/day21.txt" 

    let comp = IntCode2 ""
    let inq = comp.Initialise prog
    
    let c (i: int64) :System.Char =
        char (int i)

    let mutable finished = false

    comp.OutputReady.Add(fun output -> (

        printf "%A" (c output))
    )
          
            


    while not finished do
        async { do! Async.Sleep(100) } |> ignore


    0