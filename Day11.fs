module Day11

open Utils
open IntCode2

let day11 = 
    print "Advent of code - Day 11 - Space Police"

    let prog = readCSV "./data/day11.txt" 

    let comp = IntCode2("painter")
    let inq = comp.Initialise prog

    inq.Post(0L); // Start on black

    let mutable outCount = 0 
    let mutable finished = false

    let mutable colour = -1L
    let mutable dir = -1L
    comp.OutputReady.Add(fun o -> // System.Console.ReadKey() |> ignore
          if o = -99999L then
             finished <- true
          else 
            // Work
            
            if outCount = 0 then 
                colour <- o
                outCount <- outCount + 1
            else    
                dir <- o
                outCount <- 0

                printf "%A  %A\n" colour dir
                inq.Post(1L);   // logic here



    )


    while not finished do
        async { do! Async.Sleep(10) } |> ignore
    
    // let mutable dataReady = false
    // let mutable counter = 0
    // let mutable output = -1L, -1L
    
    // let outputFunc v =
    //     printf "Output : %A\n" v
    //     output <- if counter = 0 then
    //                 counter <- counter + 1  
    //                 v, snd output
    //               else 
    //                 dataReady <- true
    //                 counter <- 0
    //                 fst output, v
    //     printf "Output2 : %A\n" output
        
        

        


    // comp.OutputReady.Add(outputFunc)

    // inq.Post(0L); // Start on black
    
    // let mutable finished = false
    // comp.OutputReady.Add(fun o -> finished <- o = -99999L)
    // while not finished do
    //     async { 
    //         printf "%A\n" dataReady
    //         if dataReady then
    //             dataReady <- false
    //             printf "FULL OUTPUT : %A \n" output
    //         else 
    //             printf "X\n"

    //         do! Async.Sleep(10)
       
    //      } |> ignore


    // System.Console.ReadKey() |> ignore
    // System.Console.ReadKey() |> ignore
   
    0