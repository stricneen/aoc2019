module Day10

open Utils

type Point = { X:int; Y: int }

let day10 = 
    // print "Advent of code - Day 10 - Monitoring Station"

    let test =
        [|
        ".#..#";
        ".....";
        "#####";
        "....#";
        "...##"       
        |]

    // Best is 5,8 with 33 other asteroids detected:
    let test2 = 
        [|
        "......#.#.";
        "#..#.#....";
        "..#######.";
        ".#.#.###..";
        ".#..#.....";
        "..#....#.#";
        "#..#....#.";
        ".##.#..###";
        "##...#..#.";
        ".#....####"
        |]

    // Best is 1,2 with 35 other asteroids detected:
    let test3 = 
        [|
            "#.#...#.#.";
            ".###....#.";
            ".#....#...";
            "##.#.#.#.#";
            "....#.#.#.";
            ".##..###.#";
            "..#...##..";
            "..##....##";
            "......#...";
            ".####.###.";
        |]

    // Get locations of all asteroids
    let locations (matrix: string[]) =
        let w = String.length matrix.[0] - 1 
        let h = Array.length matrix - 1

        seq {
            for x in 0 .. w do
                for y in 0 .. h do
                    if matrix.[x].[y] = '#' then
                        yield { X=y; Y=x }
        }

    // let locs = locations test
    //let locs = (locations test2) |> Seq.toList
    let locs = locations (readLines "./data/day10.txt")

    let inside a b c = // c falls inside a and b
        // printf "%A %A %A \n" a b c 
        if (c.X > a.X && c.X > b.X) || (c.X < a.X && c.X < b.X)
            then false
        else if (c.Y > a.Y && c.Y > b.Y) || (c.Y < a.Y && c.Y < b.Y)
            then false
        else if a.X = c.X then 
            b.X = c.X
        else if a.Y = c.Y then
            b.Y = c.Y
        else
            (c.Y - a.Y) * (b.X - a.X) = (c.X - a.X) * (b.Y - a.Y)
 
        
    let pairs =
        seq {
        for loc1 in (locs |> Seq.toList) do
            for loc2 in (locs |> Seq.where(fun x -> x <> loc1) |> Seq.toList) do
                yield loc1, loc2
        }

    let los pair =
        let a, b = pair
        locs 
        |> Seq.where(fun c -> c <> a && c <> b)
        |> Seq.exists(fun c -> inside a b c) 
   

    //let xx = los ({X=1;Y=4}, {X=4;Y=7})
   // printf "%A\n" xx

    let visible = 
        pairs
        |> Seq.map (fun x -> x, not (los x))

    let totalVis = 
        visible
        |> Seq.where snd
        |> Seq.groupBy (fun x -> fst(fst x))
        |> Seq.map (fun x -> fst x, snd x |> Seq.length)

    let best =
        totalVis
        |> Seq.maxBy snd


    printf "x : %A\n" (visible |> Seq.where (fun x -> let a = fst x
                                                      fst a = {X=1;Y=4}) 
                               |> Seq.toList
    )


    //printf "%A\n" (totalVis |> Seq.toList)

    printf "winner : %A\n" best











    // let canSee location =
    //     for pair in pairs do
    //             let vis = all
    //                         |> Seq.where (fun x -> not (online x loc loc2) )
    //                         |> Seq.length
    //             vis, location


    //let locs = locations test


    // let los = locations
    //           |> Seq.map canSee




    //printf "%A\n" test

    //let input = readLines "./data/day10.txt" 
    //printf "%A\n" (locations input |> Seq.toList)




    0


