module Day11

open Utils
open IntCode2

type Direction = North | East | South | West

let day11 = 
    print "Advent of code - Day 11 - Space Police"

    let prog = readCSV "./data/day11.txt" 

    let comp = IntCode2("painter")
    let inq = comp.Initialise prog


    let grid = Array2D.init 100 100 (fun _ _-> 0L) 
    let mutable loc = 50, 50
    let mutable direction = Direction.North

    let mutable painted = []

    let rotate current dir =
        match current, dir with
        | North, 0 -> West
        | West, 0 -> South
        | South, 0 -> East
        | East, 0 -> North
        | North, 1 -> East
        | West, 1 -> North
        | South, 1 -> West
        | East, 1 -> South
        | _ -> failwith "Bad direction"

    let move location direction = 
        let x, y = location
        match direction with
        | North -> x, y-1
        | West -> x-1, y
        | South -> x, y+1
        | East -> x+1, y

    let painting colour rotation = 
        printf "Painting : %A  %A\n" colour direction

        // Paint current 
        let x, y = loc
        Array2D.set grid x y colour

        // Rotate
        direction <- rotate direction rotation

        // Forwad one
        loc <- move loc direction

        printf "loc : %A   dir :  %A\n" loc direction

        // System.Console.ReadKey() |> ignore

        painted <- loc :: painted

        // Get curent 
        grid.[fst loc, snd loc]

       



    inq.Post(1L); // Start on black

    let mutable outCount = 0 
    let mutable finished = false

    let mutable colour = -1L
    let mutable dir = -1L
    comp.OutputReady.Add(fun o -> // System.Console.ReadKey() |> ignore
          if o = -99999L then
             print "Finished"
             let distinct = List.distinct painted

             printf "total : %A\n" (List.length distinct)

             for x in 0 .. (Array2D.length1 grid) - 1 do
                 let line = grid.[*, x]
                            |> Array.map (fun x -> int32(x))
                            |> Array.fold (fun a e -> a + if e = 1 then e.ToString() else " " ) ""


                 printf "%A\n" line


             finished <- true
          else 
         
           // Work
            if outCount = 0 then 
                colour <- o
                outCount <- outCount + 1
            else    
                dir <- o
                outCount <- 0
                let colourToPaint = painting colour (int32(dir))
                inq.Post(colourToPaint);  
    )


    while not finished do
        async { do! Async.Sleep(10) } |> ignore
    

   
    // let white = (grid |> Array2D.iter (fun x -> if x = 1L then white <- white + 1)
    // let mutable black = 0
    // printf "W: %A\n" 
    // printf "B: %A\n" (grid |> Array2D.iter (fun x -> if x = 0L then black <- black + 1))
    
    // for x in 0 .. Array2D.length1 grid do
    //     printf "%A\n" grid.[x, *]




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
        
        

        


   
    0