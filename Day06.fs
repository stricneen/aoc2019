module Day6

open Utils

let day6 = 
    print "Advent of code - Day 6 - Universal Orbit Map"

    let input = readLines "./data/day6.txt"
    
    let orbits = input
                 |> Seq.map(fun x -> x.[0..2], x.[4..6])
                 |> Seq.toList
    
    let com = orbits |> Seq.filter(fun x -> fst x = "COM")
    
    printf "%A\n" (Seq.length orbits) 

    let total =  ResizeArray<int32>()



    let seqin planets level = 
        let moons = planets |> Seq.map snd |> Seq.toList
        //printf "moons: %A\n" moons
        let xx = orbits
                 |> List.filter (fun x -> List.contains (fst x) moons)
        
        total.Add (level * (List.length moons)) // **
        //printf "%A\n" (total |> Seq.toList)
       // let temp = System.Console.ReadKey()
        xx

    let rec al planets level =
        match planets with 
        | sequence when Seq.isEmpty sequence -> Seq.empty
        | _ ->  let x = seqin planets level
                al x (level + 1)

    let origin = seqin com 1

    let y = al origin 2

    printf "total  %A\n" (Seq.sum total)



    let rec routeToCom node lst =
        let node = orbits |> List.where (fun x -> snd x = node) |> List.head
        // print (fst node)
        
        match node with
        | (x,y) when x = "COM" -> lst
        | _ -> routeToCom (fst node) (fst node :: lst)

    let y = routeToCom "YOU" []
    print ""
    let s = routeToCom "SAN" []


    let y' = y |> List.where (fun x -> not (List.contains x s))
    let s' = s |> List.where (fun x -> not (List.contains x y))
    
    

    printf "y   %A\n" (y' |> List.length)
    printf "s   %A\n" (s' |> List.length)

    printf "%A\n"  ((y' |> List.length) + (s' |> List.length) )
    // let extract orbits root =
    //     let planets = root |> Seq.map snd
        
    //     printf "planets : %A\n" planets

    //     orbits 
    //     |> Seq.where (fun x -> Seq.contains (fst x) planets)
    //     |> printf "%A"

    //     let moons = orbits 
    //                 |> Seq.where (fun x -> Seq.contains (fst x) planets)
                  
                    
    //     printf "moons: %A\n" moons

    //     let s = set moons
    //     let rem =  orbits |> Seq.filter(fun x -> not ((s).Contains x)) //*
    //     rem, moons

// 2QB)M99
// WZ9)XC9
// COM)RR1
// F3J)G1X
// SCR)L66
// 46X)QT3
// RR1)233
// RR1)3FR


// root : COM)RR1

        
    // let x = extract orbits com
    // // print "@"


    // let rem = fst x
    // let moons = snd x
    
    // printf "%A\n" (rem |> Seq.length)

    // printf "%A\n" moons
 
    // let y = extract (fst x) (snd x)
 

    0
