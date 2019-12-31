module Day23

open Utils
open IntCode2

//type OutParam = { address: int64; x: int64; y: int64 }

let day23 = 
    print "Advent of code - Day 23 - Category Six"

    let prog = readCSV "./data/day23.txt" 

    // will be xxxxxxxaaa xxx : command,  aaa : address
    // let outputNic x = 
    //     printf "OUTPUT : %A\n" x

    let network = List.init 50 (fun x -> 
        let c = IntCode2 (x.ToString())
        let q = c.Initialise (Array.copy prog)
        //q.Post (int64 x)
        //c.OutputReady.Add(outputNic)
        (x, q, c)
    )

    network
    |> List.iteri(fun i x' -> 
        let x, q, c = x'
        

        // printf "Setting output for : %A\n" x
       // let q = c.Initialise (Array.copy prog)

        c.OutputReady.Add(fun o -> 
           // printf "OOOOOOO: %A\n" o
        
            let command = o / 1000L
           // printf "c: %A\n" command
            let address = int(o - command * 1000L)
           // printf "a: %A\n" address
            let reciept = network |> List.tryFind(fun x' -> let x,q,c = x'
                                                            x = address )

            match reciept with
            | None -> printf "Can't find address : %A\n" address
            | Some (_,y,_) -> y.Post command 

  //          System.Console.ReadKey()

            //q.Post command
            ()
        )

        q.Post (int64 x) // Address



       // let q = c.Initialise (Array.copy prog)

       // q.Post (int64 i)

    )

    // network
    // |> List.iteri(fun i x' ->

    //     let x,c,q = x'
    //     printf "Setting address for : %A\n" x
    //     q.Post (int64 i)
    // )

    // network
    // |> List.iter(fun x -> x
    //                 let c,q = x
    //                 c.OutputReady.Add(fun x -> ())
    //             )


    // printf "%A'n" network

    // let comp = IntCode2 "nic-0"
    // let inq = comp.Initialise prog
    
    // comp.OutputReady.Add(fun x -> printf "%A\n" x)
    // inq.Post 0L

    let mutable finished = false

//    comp.OutputReady.Add(fun output -> ())    

          
            


    while not finished do
        async { do! Async.Sleep(100) } |> ignore


    0