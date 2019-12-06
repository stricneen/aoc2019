module Day6

open Utils

let day6 = 
    print "Advent of code - Day 6"

    let input = readLines "./data/day1.txt"

    let data = input
               |> Seq.map(fun x -> x.[0..2], x.[4..6])


    printf "%A\n" data















    0