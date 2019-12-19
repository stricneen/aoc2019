module Day15

open Utils
open IntCode2

let day15 = 
    print "Advent of code - Day 15 - Oxygen System"

    let prog = readCSV "./data/day15.txt" 

    let comp = IntCode2("maze")
    let inq = comp.Initialise prog
    let mutable finished = false
    
    comp.OutputReady.Add(fun output ->
          if output = -99999L then
              finished <- true
          else 
              finished <- false
         )
        
    while not finished do
        async {
            do! Async.Sleep(100) } |> ignore




    0