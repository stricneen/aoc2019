module Day23

open Utils
open IntCode3

let day23 = 
    print "Advent of code - Day 23 - Category Six"

    let prog = readCSV "./data/day23.txt" 

    let mutable finished = false

    
    // will be xxxxxxxaaa xxx : command,  aaa : address
    // let outputNic x = 
    //     printf "OUTPUT : %A\n" x

    let network = List.init 50 (fun x -> 
        
        let c = IntCode3 (x.ToString())
        //let event = new System.Threading.AutoResetEvent(false)
        //c.AutoReEvent event
        let q = c.Initialise (Array.copy prog)

        // q.Post (int64 x)
        // c.OutputReady.Add(outputNic)
        (x, q, c)
    )

    network
    |> List.iteri(fun i x' -> 
        let x, q, c = x'
        
        // printf "Setting output for : %A\n" x
        // let q = c.Initialise (Array.copy prog)

        c.OutputReady.Add(fun o -> 
            let command = o / 1000L
            let address =
                if command > 0L then 
                    int(o - command * 1000L)
                else
                    1000 - int(command * 1000L - o)
             
            let reciept = network |> List.tryFind(fun x' -> let x,q,c = x'
                                                            x = address )

            match reciept with
            | None -> printf "Can't find address : %A\n" address
                      finished <- true
            | Some (_,y,_) -> y.Post command 

            ()
        )
    )

    

    // network
    // |> List.iteri(fun i x' ->
    //     let x, q, c = x'    
        // q.Post (int64 i)
   
    // )
 
  //  print "press key"
   // System.Console.ReadKey()
   
    // network
    // |> List.iteri(fun i x' ->
    //     let x, q, c = x'    
    //     c.Booted true
    // )



    while not finished do

        // print "press key"
        // System.Console.ReadKey()
        // network
        // |> List.iteri(fun i x' -> 
        //     let x, q, c, e = x'
        //     e.Set()
        //     ()
        // )

        async { do! Async.Sleep(5000) } |> ignore


    0