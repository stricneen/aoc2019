module Day9

open Utils

let day9 = 
    print "Advent of code - Day 9 - Sensor Boost"

    let prog9 = readCSV "./data/day9.txt" 

    // let x = 34915192 * 34915192

    // let t1 = toSeq "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
    // let o1 = IntCode.execute t1 [| |]
    // printf "%A\n" o1

    // let t2 = toSeq "1102,34915192,34915192,7,4,7,99,0"
    // let o2 = IntCode.execute t2 [| 1L |]
    // printf "%A\n" o2
    
    // let t3 = toSeq "104,1125899906842624,99" 
    // let o3 = IntCode.execute t3 [| 1L |]
    // printf "%A\n" o3

    
    let o = IntCode.execute prog9 [| 2L |]
    //printf "%A\n" o

    // 3484538123







    0