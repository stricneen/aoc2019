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
        




    let mutable counter = 0L
    let mutable finished = false
    let mutable collecting = false

    let scaffold = Array2D.create 85 85 ' '

    let mutable x = 0
    let mutable y = 0

    // let completed = 
    //     printf "tt: %A\n" scaffold
    
    comp.OutputReady.Add(fun output ->

        if output = 10L then
            let row = scaffold.[*, y] |> String
            print row
            
            x <- 0
            y <- y + 1
            // printn y
            

            if y = 42 then
                print "done yay!"
                //printf "tt: %A\n" scaffold
                let o = findIntersection scaffold
                        |> Seq.sum
                printn o
                finished <- true
                //completed
        else
            Array2D.set scaffold x y (char output) 
            x <- x + 1

    )    
    


   
    
    while not finished do
        async {
            do! Async.Sleep(100) } |> ignore


    0