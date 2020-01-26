module Day21

open Utils
open IntCode2


let day21 =
    print "Advent of code - Day 21 - Springdroid Adventure"

    let prog = readCSV "./data/day21.txt" 

    let comp = IntCode2 "bot"
    let inq = comp.Initialise prog
    
    let mutable finished = false
    let mutable buffer = ""

 //   inq.Post 30L
    
    comp.OutputReady.Add(fun output -> 
        if output = 10L then
            print buffer
            //print "\n"
            buffer <- ""
        else
            buffer <- buffer + (char(output |> int)).ToString()
        )
          
            


    while not finished do
        let command = System.Console.ReadLine()
        //print command
        for x in command do
            let i = (int64 x)
            //let i' = if i = 13L then 10L else i
            //printf "%A\n" i'
            inq.Post i
        inq.Post 10L
        async { 
            
            do! Async.Sleep(100) } |> ignore


    0

