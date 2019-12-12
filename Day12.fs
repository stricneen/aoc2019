module Day12

open Utils

type Pos = { X:int; Y:int; Z:int}

let day12 = 
    print "Advent of code - Day 12 - The N-Body Problem"

// <x=1, y=-4, z=3>
// <x=-14, y=9, z=-4>
// <x=-4, y=-6, z=7>
// <x=6, y=-9, z=-11>


    let io = { X=1; Y= -4; Z=3 }
    let europa = { X= -14; Y=9; Z= -4 }
    let ganymede = { X= -4; Y= -6; Z=7 }
    let callisto = { X=6; Y= -9; Z= -11 }
    
    let start = [| io, europa, ganymede, callisto |]

    let a = { X= -1; Y=0; Z=2 }
    let b = { X=2; Y= -10; Z= -7 }
    let c = { X=4; Y= -8; Z=8 }
    let d = { X=3; Y=5; Z= -1 }

    let s = [
        a, {X=0;Y=0;Z=0} ;
        b, {X=0;Y=0;Z=0} ;
        c, {X=0;Y=0;Z=0} ;
        d, {X=0;Y=0;Z=0} ;
    ]

    let pairs = comb 2 s

    let n a b c =
        if a < b then
            c - 1
        else if a > b then
            c + 1
        else 
            c
          

    let applyVelocity (l: (Pos * Pos) list) =
       // let posa, vela = a.[0]
       // let posb, velb = b.[0]
        //printf "%A\n\n" a
       let a  = fst l.[0]
       let a' = snd l.[0]
       let b  = fst l.[1]
       let b' = snd l.[1]
       
       [ 
           (a, {X=n a.X b.X a'.X ;Y=n a.Y b.Y a'.Y;Z=n a.Z b.Z a'.Z}),
           (b, {X=n a.X b.X b'.X ;Y=n a.Y b.Y b'.Y;Z=n a.Z b.Z b'.Z})
       ]
        

    //let pairs =
    //let x = 

    //for x in 1 .. 10 do 
    let y = comb 2 s
            |> List.map applyVelocity

    printf "%A\n\n" y
        




    0