module Day24

open Utils
open IntCode2

let day24 = 
    print "Advent of code - Day 24 - Planet of Discord"

    let read2DArray path = 
        let input = readLines path
        let a2d = array2D input
        a2d
        
    let prog = read2DArray "./data/day24.txt"

    let hasAbove (grid: char[,]) x y =
        x > 0 && grid.[x - 1, y] = '#'

    let hasBelow grid x y = 
        x < Array2D.length1 grid - 1 && grid.[x + 1, y] = '#'

    let hasLeft  (grid: char[,]) x y =
        y > 0 && grid.[x, y - 1] = '#'

    let hasRight grid x y = 
        y < Array2D.length2 grid - 1 && grid.[x, y + 1] = '#'

    let isBug (grid: char[,]) x y =
        grid.[x,y] = '#'

    let adjCount grid x y =
        [
            hasAbove grid x y;
            hasBelow grid x y;
            hasLeft grid x y;
            hasRight grid x y
        ]
        |> List.sumBy (fun x -> if x then 1 else 0)

    let toBeBug grid x y =
        let adj = adjCount grid x y
        match adj with
        | 1 when isBug grid x y -> '#'
        | 1 when not (isBug grid x y) -> '#'
        | 2 when not (isBug grid x y) -> '#'
        | _ -> '.'
        
        
    let ite grid = 
        grid
        |> Array2D.mapi (fun x y e -> toBeBug grid x y )
        

    let rec iterate grid c = 
        printf "%A\n\n" grid
        System.Console.ReadKey() |> ignore
        let newGrid = ite grid
        iterate newGrid c+1


    let x = iterate prog 0

    // printf "%A\n" prog

    // let one = ite prog

    // printf "%A\n" one

    0

// ....#
// #..#.
// #..##
// ..#..
// #....