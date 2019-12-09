module Day7

open Utils

let day7 = 
    print "Advent of code - Day 7 - Amplification Circuit"

    let prog7 = readCSV "./data/day7.txt"
    let prog5 = readCSV "./data/day5.txt"
    

    // let x = perms [0;1;2;3;4] 
    // printf "%A\n" (x |> Seq.toList)

//  Max thruster signal 43210 (from phase setting sequence 4,3,2,1,0):

//  3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0

    let input = toSeq "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"

    let o1 = IntCode.execute input [| 4; 0 |]
    let o2 = IntCode.execute input [| 3; o1 |]
    let o3 = IntCode.execute input [| 2; o2 |]
    let o4 = IntCode.execute input [| 1; o3 |]
    let o5 = IntCode.execute input [| 0; o4 |]
    print "***********"
    printn o5
    print "***********"



    // DAY 5
    // let output = IntCode.execute prog5 [| 1 |]
    // print "***********"
    // printn output
    // print "***********"

    0