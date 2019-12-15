module Day11

open Utils

let day11 = 
    print "Advent of code - Day 10 - Space Police"

    let prog = readCSV "./data/day9.txt" 

    let o = IntCode.execute prog [| 2L |]
    
    0