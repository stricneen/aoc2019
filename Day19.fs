module Day19

open Utils
open IntCode2

let day19 = 
    print "Advent of code - Day 19 - Tractor Beam"

    // let prog = readCSV "./data/day19.txt" 

    // let comp = IntCode2("beam")
    // let inq = comp.Initialise prog

    let mutable counter = 0L
    let mutable finished = false
    // let mutable x = 0
    // let mutable y = 0

    // let locs = 
    //     seq {
    //         for x in 0L .. 49L do
    //             for y in 0L .. 49L do
    //                 yield x,y
    //     }

    let comp = IntCode2("test")
    let prog = readCSV "./data/day19.txt" 
    let inq = comp.Initialise prog

    comp.OutputReady.Add(fun output ->

    // System.Console.ReadKey()
    // print ("Output : " + output.ToString())
        if output <> -99999L then 
         //   printAt x y (output.ToString())
            counter <- counter + output
         //   print ("Counter : " + counter.ToString())
        else 
            System.Console.ReadKey() |> ignore
        ()
    )    
    // for x in 0 .. 9 do
    //     for y in 0 .. 9 do
    //         let comp = IntCode2(x.ToString() + " : " + y.ToString())
    //         let prog = readCSV "./data/day19.txt" 

    //         comp.OutputReady.Add(fun output ->

    //        // System.Console.ReadKey()
    //            // print ("Output : " + output.ToString())
    //             if output <> -99999L then 
    //                 printAt x y (output.ToString())
    //                 counter <- counter + output
    //              //   print ("Counter : " + counter.ToString())
    //             else 
    //                 System.Console.ReadKey() |> ignore
    //             ()
    //         )    
    //         let inq = comp.Initialise prog
    //         // inq.Post(int64(x))
    //         // inq.Post(int64(y))
    //         inq.Post(int64(x))
    //         inq.Post(int64(y))



          //  printAt x y "*"
         

    // for l in locs do
    //     inq.Post(fst l)
    //     inq.Post(snd l)
    //     printAt 55 10 ((fst l).ToString() + " : " + (snd l).ToString())


   


    
    
    
    while not finished do
        async { do! Async.Sleep(100) } |> ignore


    0