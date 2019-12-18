module Day7

open Utils
open IntCode2

let day7 = 
    print "Advent of code - Day 7 - Amplification Circuit"

    // let queueFactory = 
    //     let outputQueue = MailboxProcessor<int64>.Start(fun inbox -> 
    //         let rec messageLoop() = async{
    //             let! msg = inbox.Receive()
    //             printfn "message is: %A\n" msg
    //             return! messageLoop()  
    //             }
    //         messageLoop() 
    //         )
    //     outputQueue

    let amplify prog (phases: int64[]) = 

       
        let ampa = IntCode2("A")
        let inQa = ampa.Initialise prog
        inQa.Post phases.[0]
        inQa.Post 0L

        let ampb = IntCode2("B")
        let inQb = ampb.Initialise prog
        inQb.Post phases.[1]
        
        let ampc = IntCode2("C")
        let inQc = ampc.Initialise prog
        inQc.Post phases.[2]

        let ampd = IntCode2("D")
        let inQd = ampd.Initialise prog
        inQd.Post phases.[3]

        let ampe = IntCode2("E")
        let inQe = ampe.Initialise prog
        inQe.Post phases.[4]

        ampa.OutputReady.Add(inQb.Post)
        ampb.OutputReady.Add(inQc.Post)
        ampc.OutputReady.Add(inQd.Post)
        ampd.OutputReady.Add(inQe.Post)
        //ampe.OutputReady.Add(inQa.Post)
        
        

        let mutable finished = false
        let mutable output = 0L
        ampe.OutputReady.Add(fun o -> // System.Console.ReadKey() |> ignore
                                      if o = -99999L then
                                         finished <- true
                                      else 
                                          inQa.Post o
                                          output <- o )
        while not finished do
            async { do! Async.Sleep(10) } |> ignore

        output
   
    let tryPhases prog = 
        let phases = perms [9L;8L;7L;6L;5L] 
        let timings  = phases
                       |> Seq.map (fun x -> x, amplify prog (x |> Seq.toArray))
        timings


    let prog7 = readCSV "./data/day7.txt" 

    let test2 = toSeq "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"

    let output = tryPhases prog7 
                 |> Seq.maxBy snd

    print "***********"
    printf "%A\n" output
    print "***********"
    System.Console.ReadKey() |> ignore
