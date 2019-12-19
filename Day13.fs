module Day13

open System
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

    let mutable paddleX = 0
    let mutable ballX = 10
    let mutable direction = 0L
    let mutable score = 0

    let printAt x y (message: string) = 
        Console.SetCursorPosition(x, y)
        Console.Write(message)

    let drawOutput pos block =
        if fst pos = -1 then
            score <- score + block
            printAt 10 30 ("Score : " + block.ToString())
        else
            let block = 
                match block with
                | 0 -> " "
                | 1 -> "#"
                | 2 -> "*"
                | 3 -> // Console.ReadKey()
                       paddleX <- fst pos
                       "-"
                | 4 -> //Console.ReadKey()
                       ballX <- fst pos
                       "o"
                | _ -> failwith "Invalid block"

            printAt 10 28 (paddleX.ToString() + " : " + ballX.ToString())
            printAt (fst pos) (snd pos) (block.ToString())

            // if paddleX > ballX then
            //     inq.Post -1L
            // else if paddleX < ballX then
            //     inq.Post 1L
            // else 
            //     inq.Post 0L

            // System.Threading.Thread.Sleep(100)
        
   
    let mutable x = 0
    let mutable y = 0
    let mutable z = 0

    let mutable bcks = 0
    
    comp.OutputReady.Add(fun output -> // System.Console.ReadKey() |> ignore
          if output = -99999L then
              // printf "Block : %A\n" bcks
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

       
        
                if z = 2 then   
                    bcks <- bcks + 1
           
          
            

             
                    // let key = Console.ReadKey()
         
                        
            //                 if Console.KeyAvailable && Console.ReadKey().Key = ConsoleKey.LeftArrow then 
            //     //print "L"  
            //     inq.Post(-1L)
            // else if Console.KeyAvailable && Console.ReadKey().Key = ConsoleKey.RightArrow then 
            //     //print "R"  
            //     inq.Post(1L)
            // else   
            //     print "0"  
            //         inq.Post(0L)
    )

// 0 is an empty tile. No game object appears in this tile.
// 1 is a wall tile. Walls are indestructible barriers.
// 2 is a block tile. Blocks can be broken by the ball.
// 3 is a horizontal paddle tile. The paddle is indestructible.
// 4 is a ball tile. The ball moves diagonally and bounces off objects.

        

    while not finished do
//        Console.ReadKey() |> ignore
        if paddleX > ballX then
            inq.Post(-1L)
        else if paddleX < ballX then
            inq.Post(1L)
        else 
            inq.Post(0L)
        
        System.Threading.Thread.Sleep 600
        
        // async {
            
        //     do! Async.Sleep(100) } |> ignore


    print ""
    print ""
    printf "Score : %A\n" score
    Console.ReadKey() |> ignore
    0