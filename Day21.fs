module Day21

open Utils
open IntCode2

(*
OR A J      #####.###########
OR B J      #####...#########
OR D J      #####..#.########
            #####.##.########
            #####.#..########


NOT B T
AND A T
AND D T
NOT A J
OR T J
WALK



NOT D T
NOT T J

NOT A J     #####..#.########
NOT B J     #####...#########
NOT C J     #####...#########
NOT D J     #####.###########

OR A J      #####.###########
OR B J      #####...#########
OR C J      #####...#########
OR D J      #####..#.########

NOT B T     #####..#.########  *
AND C J
NOT A T
OR T J

NOT B T     #####.##.########
NOT C J
AND T J
AND A J
AND D J
NOT A T
OR T J


NOT A T     #####...#########
NOT C J
OR T J           

NOT A T     #####.###########
NOT D J
AND T J


*)




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

