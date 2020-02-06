module Day19

open Utils
open IntCode3

let day19 = 
    print "Advent of code - Day 19 - Tractor Beam"

    let mutable counter = 0L
    let mutable finished = false
    
    for x in 0L .. 49L do
        for y in 0L .. 49L do
            let comp = IntCode3("")
            let prog = readCSV "./data/day19.txt" 
            let inq = comp.Initialise prog

            comp.OutputReady.Add(fun output ->

               // let output, tag = opt
                if output <> -99999L then 
                 //   printAt x y (output.ToString())
                    let x,y = comp.GetTag
                    printAt (int y) (int x) (if output = 0L then "." else "#")
                    counter <- counter + output
                 //   print ("Counter : " + counter.ToString())
                else 
                    finished <- true
                    printf "Count : %A\n" counter
                    //System.Console.ReadKey() |> ignore
                ()
            )    

            comp.SetTag (x,y)
            inq.Post x
            inq.Post y
            


    while not finished do
        async { do! Async.Sleep(100) } |> ignore


    0