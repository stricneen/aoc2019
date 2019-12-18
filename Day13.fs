module Day13

open Utils
open IntCode2

let day13 = 
    print "Advent of code - Day 13 - Care Package"

    // let prog = readCSV "./data/day11.txt" 

    let prog = readCSV "./data/day13.txt" 

    let comp = IntCode2("game")
    let inq = comp.Initialise prog

    let mutable outCount = 0 
    let mutable finished = false

    let drawOutput pos block =
        let block = 
            match block with
            | 0 -> " "
            | 1 -> "W"
            | 2 -> "B"
            | 3 -> "-"
            | 4 -> "o"
            | _ -> failwith "Invalid block"
        System.Console.SetCursorPosition pos
        System.Console.Write(block)
        
            
    let mutable x = 0
    let mutable y = 0
    let mutable z = 0
    
    comp.OutputReady.Add(fun output -> // System.Console.ReadKey() |> ignore
          if output = -99999L then
              finished <- true
          else 
             
            // Work
            if outCount = 0 then 
                x <- int32(output)
                outCount <- outCount + 1
            else if outCount = 1 then
                y <- int32(output)
                outCount <- outCount + 1
            else 
                z <- int32(output)
                drawOutput (x, y) z
                outCount <- 0
             
                
                  
    )

// 0 is an empty tile. No game object appears in this tile.
// 1 is a wall tile. Walls are indestructible barriers.
// 2 is a block tile. Blocks can be broken by the ball.
// 3 is a horizontal paddle tile. The paddle is indestructible.
// 4 is a ball tile. The ball moves diagonally and bounces off objects.

    while not finished do
        async { do! Async.Sleep(10) } |> ignore
    
    0