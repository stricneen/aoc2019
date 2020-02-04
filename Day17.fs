module Day17

open System
open Utils
open IntCode2

let day17 = 
    print "Advent of code - Day 19 - Set and Forget"

    let prog = readCSV "./data/day17.txt" 

    let comp = IntCode2 "camera"
    let inq = comp.Initialise prog
    let hash = '#'

    let cross x y (arr:char[,]) = 
        arr.[x,y] = hash && arr.[x-1,y] = hash && arr.[x,y-1] = hash && arr.[x+1,y] = hash && arr.[x-1,y] = hash

    let findIntersection arr = seq {
        for x in 1 .. (Array2D.length1 arr) - 2 do
            for y in 1 .. (Array2D.length2 arr) - 2 do
                if arr.[x,y] = hash then
                    if cross x y arr then
                        printf "%A , %A  \n" x y
                        yield x * y
    }
        
    let mutable finished = false

    let scaffold = Array2D.create 85 85 ' '


    let mutable buffer = ""

    comp.OutputReady.Add(fun output -> 
        if output = 10L then
            print buffer
            //print "\n"
            buffer <- ""
        else
            buffer <- buffer + (char(output |> int)).ToString()
        )



    //let mutable x = 0
    //let mutable y = 0
    // comp.OutputReady.Add(fun output ->
    //     DAY 1
    //     if output = 10L then
    //         let row = scaffold.[*, y] |> String
    //         print row   
    //         x <- 0
    //         y <- y + 1
    //         if y = 42 then
    //             print "done yay!"
    //             let o = findIntersection scaffold
    //                     |> Seq.sum
    //             printn o
    //             finished <- true
    //     else
    //         Array2D.set scaffold x y (char output) 
    //         x <- x + 1
    // )    

    let toIn commands = 

        let inline charToInt c = int64 c
        print commands
        print ""

        commands//.Split(',')
        |> Seq.iter(fun x ->
            let x' = charToInt x

            printf "%A\n" x'
            inq.Post x'
            //inq.Post 44L // comma
        )
        printf "%A\n" 10L
        inq.Post 10L // newline

    
    // Main routine: A,B,C,B,A,C
    // (ASCII input: 65, 44, 66, 44, 67, 44, 66, 44, 65, 44, 67, 10)
    // Function A:   R,8,R,8
    // (ASCII input: 82, 44, 56, 44, 82, 44, 56, 10)
    // Function B:   R,4,R,4,R,8
    // (ASCII input: 82, 44, 52, 44, 82, 44, 52, 44, 82, 44, 56, 10)
    // Function C:   L,6,L,2
    // (ASCII input: 76, 44, 54, 44, 76, 44, 50, 10)
    Console.ReadKey() |> ignore
    "A" |> toIn
    "R,6,L,10,R,8,R,8,R,9" |> toIn
    "R" |> toIn
    "R" |> toIn
    "n" |> toIn


   
    
    while not finished do
        async {
            do! Async.Sleep(100) } |> ignore


    0