module Day7

open Utils

let day7 = 
    print "Advent of code - Day 7 - Amplification Circuit"

    let outputVal x = x


    let queueFactory = 
        let outputQueue = MailboxProcessor<int64>.Start(fun inbox -> 
            let rec messageLoop() = async{
                let! msg = inbox.Receive()
                printfn "message is: %A\n" msg
                return! messageLoop()  
                }
            messageLoop() 
            )
        outputQueue

    let amplify prog (phases: int64[]) = 

        let outQ1 = queueFactory
        let inQ1 = IntCode2.initialise prog outQ1
        System.Console.ReadKey() |> ignore
        inQ1.Post phases.[0]
        System.Console.ReadKey() |> ignore
        inQ1.Post 0L
        System.Console.ReadKey() |> ignore
        print "boo"

        let output = ( async {
                 let! i = outQ1.Receive()
                 return i } |> Async.RunSynchronously)
        output
 
        // let outQ2 = queueFactory
        // let inQ2 = IntCode2.initialise prog outQ2
        // inQ2.Post phases.[1]
        // inQ2.Post ( async {
        //         let! i = outQ1.Receive()
        //         return i } |> Async.RunSynchronously)

        // let outQ3 = queueFactory
        // let inQ3 = IntCode2.initialise prog outQ3
        // inQ3.Post phases.[2]
        // inQ3.Post ( async {
        //         let! i = outQ2.Receive()
        //         return i } |> Async.RunSynchronously)

        // let outQ4 = queueFactory
        // let inQ4 = IntCode2.initialise prog outQ4
        // inQ4.Post phases.[3]
        // inQ4.Post ( async {
        //         let! i = outQ3.Receive()
        //         return i } |> Async.RunSynchronously)

        // let outQ5 = queueFactory
        // let inQ5 = IntCode2.initialise prog outQ5
        // inQ5.Post phases.[4]
        // inQ5.Post ( async {
        //         let! i = outQ4.Receive()
        //         return i } |> Async.RunSynchronously)

        // let output = ( async {
        //         let! i = outQ5.Receive()
        //         return i } |> Async.RunSynchronously)
        // output


    let tryPhases prog = 
        let phases = perms [0L;1L;2L;3L;4L] 
        let timings  = phases
                       |> Seq.map (fun x -> x, amplify prog (x |> Seq.toArray))
        timings


    let prog7 = readCSV "./data/day7.txt" 

    let output = tryPhases prog7 
                  |> Seq.maxBy snd

    print "***********"
    printf "%A\n" output
    print "***********"

// thak4eeg8chaew3Y

// SonarQube-VM01

    //print "Part 2"

    // let prog7 = readCSV "./data/day7.txt" 
    // let inQueue1 = IntCode2.initialise prog7

    // inQueue1.Post 7L


    // let rec feedback prog (phases: int[]) init = 
        
        

    //     IntCode.execute (prog |> List.toArray) [| phases.[0] |]
        
    //     //IntCode.execute (prog |> List.toArray) [| phases.[1] |] infun outfun
        
        
    //     // let o3 = IntCode.execute (prog |> List.toArray) [| phases.[2]; o2 |]
    //     // let o4 = IntCode.execute (prog |> List.toArray) [| phases.[3]; o3 |]
    //     // let o5 = IntCode.execute (prog |> List.toArray) [| phases.[4]; o4 |]
    //     //feedback prog phases o5

        
    // let tryFeeback prog = 
    //     let phases = perms [9; 8; 7; 6; 5;] 
    //     let timings  = phases
    //                    |> Seq.map (fun x -> x, feedback prog (x |> Seq.toArray))
    //     timings



    // let input = toSeq "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"
    //             |> Seq.toList
                      
    // let output = feedback input [|9; 8; 7; 6; 5|] 0

    // // let output = tryFeeback input 
    // //               |> Seq.maxBy snd

    // print "***********"
    // printf "%A\n" output
    // print "***********"











    // let output = IntCode.execute prog7 [|1; 3 |]
    // print "***********"
    // printn output
    // print "***********"  


    // let phases = perms [0;1;2;3;4] 

    // let timings  = phases
    //                |> Seq.map (fun x -> amplify prog7 (x |> Seq.toArray))

    // print "** DAY 7 **"
    // printf "%A\n" timings
    // print "***********"



    
    // //  Max thruster signal 43210 (from phase setting sequence 4,3,2,1,0):
    // //  3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0
    // let input = toSeq "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
    // let output = tryPhases input
    //               |> Seq.maxBy snd

    // print "***********"
    // printf "%A\n" output
    // print "***********"



    // // Max thruster signal 54321 (from phase setting sequence 0,1,2,3,4):
    // // 3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0
    // let input = toSeq "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"
    // let output = tryPhases input
    //               |> Seq.maxBy snd

    // print "***********"
    // printf "%A\n" output
    // print "***********"

    // // Max thruster signal 65210 (from phase setting sequence 1,0,4,3,2):
    // // 3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0
    // let input = toSeq "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"
    // let output = tryPhases input
    //               |> Seq.maxBy snd

    // print "***********"
    // printf "%A\n" output
    // print "***********"



    // // DAY 5
    // let prog5 = readCSV "./data/day5.txt"
                
    // let output = IntCode.execute prog5 [| 1L |]
    // print "***********"
    // printn output
    // print "***********"

    0