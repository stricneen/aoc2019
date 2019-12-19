module Day17

open Utils
open IntCode2

let day17 = 
    print "Advent of code - Day 19 - Set and Forget"

    let prog = readCSV "./data/day19.txt" 

    let comp = IntCode2("beam")
    let inq = comp.Initialise prog


    let mutable counter = 0L
    let mutable finished = false

    comp.OutputReady.Add(fun output ->

        counter <- counter + output

    )    
    
    
    
    while not finished do
        async {
            do! Async.Sleep(100) } |> ignore


    0