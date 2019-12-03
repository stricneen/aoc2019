module Day3

open Utils

let day3 = 
    print "Advent of code - Day 3"

    let input = readLines "./data/day3.txt"

    let wire1 = input |> Seq.head |> Utils.split
    let wire2 = input |> Seq.skip 1 |> Seq.head |> Utils.split

    let test1 = "R5,U5"  //,D30,R83,U83,L12,D49,R71,U7,L72"
    let test2 = "U62,R66,U55,R34,D71,R55,D58,R83"
    
    let wire1 = test1 |> Utils.split
    let wire2 = test2 |> Utils.split

        //        (340, 430)  L43
    let route start vector = 
        let splitv (dir:string) = 
            (dir.[0], dir.[1..] |> System.Int32.Parse)
        let inst = splitv vector
        match fst inst with
        | 'U' -> seq { for v in snd start .. (snd start + snd inst) do  (fst start, v) }
        | 'D' -> seq { for v in snd start .. -1 .. (snd start - snd inst) do (fst start, v) }
        | 'L' -> seq { for v in fst start .. -1 .. (fst start - snd inst) do (v, snd start) }
        | 'R' -> seq { for v in fst start .. (fst start + snd inst) do  (v, snd start) }
        | _ -> Seq.empty

    let s = seq { yield (0,0)}

    // let x  = route (2,2) "R5" 
    //          |> Seq.toList
    // printf "%A" x


    let folder acc elem = 
        let start = acc |> Seq.last

        printf "acc %A\n" (acc |> Seq.toList)
        
        let r = route start elem
        
        printf "r %A\n" (r |> Seq.toList)

        printf "out %A\n" (acc |> Seq.toList)

        acc 
    

    let coords wire =
        wire
        |> Seq.fold folder s

    //let y = route (0,0) "L43"
    
    let x = coords wire1 |> Seq.toList
    printf "%A" x
    
    // let y = coords wire2

    // let cross = intersect x y
    // printf "%A" cross
    
    //y |> Seq.iter (fun x -> printf "%a , %a" (fst x) (snd x))
    
    //printn (y |> Seq.length)





    //let coords1 = coords wire1
    //let coords1 = coords wire1
    






    ()