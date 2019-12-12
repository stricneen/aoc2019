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

    let s = [| 
        a, {X=0;Y=0;Z=0} ;
        b, {X=0;Y=0;Z=0} ;
        c, {X=0;Y=0;Z=0} ;
        d, {X=0;Y=0;Z=0} ;
    |]

    0