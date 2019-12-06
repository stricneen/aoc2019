module Day6

open Utils

let day6 = 
    print "Advent of code - Day 6"

    let input = readLines "./data/day6.txt"

    let data = input
               |> Seq.map(fun x -> x.[0..2], x.[4..6])

    let com = data |> Seq.filter(fun x -> fst x = "COM")

    printf "%A\n" com
    printf "%A\n" data















    0