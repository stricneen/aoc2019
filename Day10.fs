module Day10

open Utils

let day10 = 
    print "Advent of code - Day 10 - Monitoring Station"

    let test =
        [|
        ".#..#";
        ".....";
        "#####";
        "....#";
        "...##"       
        |]

    let locations matrix =
        let w = String.length matrix.[0]
        let h = Array.length matrix

        seq {
        for x in 0 .. w do
            for y in 0 .. h do
                if matrix.[y][x] = "#" then
                    yield (x, y)
        }
        
    let locs = locations test

    for v in locs do
        printf "%A\n" v


    printf "%A\n" test

    // let input = readLines "./data/day10.txt" 
    // printf "%A\n" input




    0


