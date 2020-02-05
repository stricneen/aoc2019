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
        else if output > 255L then
            pl output
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

    Console.ReadKey() |> ignore
    "A,B,A,C,B,C,A,B,A,C" |> toIn
    "R,6,L,10,R,8,R,8" |> toIn
    "R,12,L,8,L,10" |> toIn
    "R,12,L,10,R,6,L,10" |> toIn   
    "n" |> toIn

// R6 L10 R8 R8 R12 L8 L10 R6 L10 R8 R8 R12 L10 R6 L10 R12 L8 L10 R12 L10 R6 L10 R6 L10 R8 R8 R12 L8 L10 R6
// R6 L10 R8 R8            R6 L10 R8 R8                                          R6 L10 R8 R8   
//              R12 L8 L10                            R12 L8 L10                              R12 L8 L10
//                                     R12 L10 R6 L10             R12 L10 R6 L10

    while not finished do
        async {
            do! Async.Sleep(100) } |> ignore


    0