module Day7

open Utils

let day7 = 
    print "Advent of code - Day 7 - Amplification Circuit"



    let amplify prog (phases: int[]) = 
        let o1 = IntCode.execute prog [| phases.[0]; 0 |]
        let o2 = IntCode.execute prog [| phases.[1]; o1 |]
        let o3 = IntCode.execute prog [| phases.[2]; o2 |]
        let o4 = IntCode.execute prog [| phases.[3]; o3 |]
        let o5 = IntCode.execute prog [| phases.[4]; o4 |]
        o5

    let tryPhases prog = 
        let phases = perms [0;1;2;3;4] 
        let timings  = phases
                       |> Seq.map (fun x -> x, amplify prog (x |> Seq.toArray))
        timings


    let prog7 = readCSV "./data/day7.txt" 



    let output = tryPhases prog7 
                  |> Seq.maxBy snd

    print "***********"
    printf "%A\n" output
    print "***********"

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



    // DAY 5
    // let prog5 = readCSV "./data/day5.txt"
    // let output = IntCode.execute prog5 [| 1 |]
    // print "***********"
    // printn output
    // print "***********"

    0