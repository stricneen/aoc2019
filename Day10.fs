module Day10

open System
open Utils

type Point = { X:int; Y: int }

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

    // can A see B using locs
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


    // printf "x : %A\n" (visible |> Seq.where (fun x -> let a = fst x
    //                                                   fst a = {X=1;Y=4}) 
    //                            |> Seq.toList
    // )


    //printf "%A\n" (totalVis |> Seq.toList)

    // printf "winner : %A\n" best


    print "Part 2"

 


    let center = { X=30; Y=34 }
//    let center = { X=0; Y=0 }

    let angle center x =
            (atan ( abs(float(x.Y - center.Y)) / abs(float(x.X - center.X )))) 
        // atan2 (float center.Y) (float center.X) - atan2 (float x.Y) (float x.X) 
  //      atan ( float(x.Y - center.Y) / float(x.X - center.X) ) / 180. * System.Math.PI
          
    let angle2 x y = 
        let  z = (atan2  (float (y.Y - x.Y)) (float (y.X - x.X))) //* 180. / Math.PI;
        let t = if z > 0. then  z else  (2.* Math.PI + z) * 360. / (2.* Math.PI)
        ((t + 90.) % 360.)

    let mutable c = 0

    // printf "locations : %A\n" (locs |> Seq.length)
    
    let angles = locs
                   // |> Seq.map (fun x -> { X=x.X - 30;Y=x.Y-34  })
                    |> Seq.where (fun x -> x <> center)
                    |> Seq.where (fun x -> not (los (center, x)))
                    |> Seq.map (fun x -> x, angle2 center x)
                    |> Seq.sortBy snd
                    |> Seq.map (fun x -> c <- c + 1
                                         c, x)
                    // |> Seq.where (fun x -> let a,b = x 
                    //                        (fst b) = { X=30;Y=33 } )
                                            

    for x in angles do
        printf "%A\n" x
            // { X = 30, Y = 34 }   can see 344



// https://stackoverflow.com/questions/1311049/how-to-map-atan2-to-degrees-0-360

// 2641



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


