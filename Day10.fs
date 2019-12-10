module Day10

open Utils

let day10 = 
    // print "Advent of code - Day 10 - Monitoring Station"

    let test =
        [|
        ".#..##";
        "......";
        "#####.";
        "....#.";
        "...##."       
        |]

    let locations (matrix: string[]) =
        let w = String.length matrix.[0] - 1 
        let h = Array.length matrix - 1

        seq {
            for x in 0 .. h do
                for y in 0 .. w do
                    if matrix.[x].[y] = '#' then
                        yield (x, y)
        }
        
    let locs = locations test

    for v in locs do
        printf "%A\n" v


   //printf "%A\n" test

    // let input = readLines "./data/day10.txt" 
    // printf "%A\n" input




    0


