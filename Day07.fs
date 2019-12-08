module Day7

open Utils

let day7 = 
    print "Advent of code - Day 7 - Amplification Circuit"

    let prog7 = readCSV "./data/day7.txt"
    let prog5 = readCSV "./data/day5.txt"
    


    let output = IntCode.execute prog5 [| 1 |]

    print "***********"
    printn output
    print "***********"

    0