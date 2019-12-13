module Day12

open Utils

type Pos = { X:int; Y:int; Z:int }

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

    // Eg 1
    let a = { X= -1; Y=0; Z=2 }
    let b = { X=2; Y= -10; Z= -7 }
    let c = { X=4; Y= -8; Z=8 }
    let d = { X=3; Y=5; Z= -1 }

// <x=-8, y=-10, z=0>
// <x=5, y=5, z=10>
// <x=2, y=-7, z=3>
// <x=9, y=-8, z=-3>
    

    // Eg 2
    // let a = { X= -8; Y= -10; Z=0 }
    // let b = { X=5; Y=5; Z=10 }
    // let c = { X= 2; Y= -7; Z=3 }
    // let d = { X= 9; Y= -8; Z= -3 }

    let s = [
        a, {X=0;Y=0;Z=0};
        b, {X=0;Y=0;Z=0};
        c, {X=0;Y=0;Z=0};
        d, {X=0;Y=0;Z=0};
    ]

    // let pairs = comb 2 s

    let n a b c =
        if a < b then
            1
        else if a > b then
           -1
        else 
            0
          

    let applyVelocity (l: (Pos * Pos) list) =
       // printf "A : %A\n\n" l
       let a  = fst l.[0] 
       let a' = snd l.[0]
       let b  = fst l.[1]
       let b' = snd l.[1]
       let v = [ 
           (a, a', {X = n a.X b.X a'.X; Y = n a.Y b.Y a'.Y; Z = n a.Z b.Z a'.Z}),
           (b, b', {X = n b.X a.X b'.X; Y = n b.Y a.Y b'.Y; Z = n b.Z a.Z b'.Z})
       ]
       // printf "B : %A\n\n" v
       v
        
    let unwrapPairs acc ele =
        let acc1 = fst ele :: acc
        snd ele :: acc1

    let sumVelocity acc (ele: Pos * Pos * Pos) = 
        // printf "acc : %A\n\n" acc
        // printf "ele : %A\n\n" ele
        
        
        let pos, vel, del = ele

        let x = acc |> List.where (fun x -> fst x = fst3 ele)
        if List.isEmpty x then 
            (pos, { X = vel.X + del.X ; Y = vel.Y + del.Y; Z = vel.Z + del.Z}) :: acc
        else 
            let nw = acc |> List.where (fun x -> fst x <> fst3 ele)
            let vel' = snd (List.head x)
            (pos, { X = vel'.X + del.X ; Y = vel'.Y + del.Y; Z = vel'.Z + del.Z}) :: nw

    let applyGravity data =
        let pos, vel = data
        { X = pos.X + vel.X; Y = pos.Y + vel.Y; Z = pos.Z + vel.Z }, vel

    //for x in 1 .. 10 do 
    let tick state =
        comb 2 state
                |> List.map applyVelocity
                |> List.concat
                |> List.fold unwrapPairs []
                |> List.fold sumVelocity []
                |> List.map applyGravity


    let interateSelf func initial count =
        let rec inner intermediate n =
            if n = 1 then
                func intermediate
                    else
                inner (func intermediate) (n - 1)

        inner initial count

  
    let iter func (input: 't) n = 
        let rec inner (input: 't) count = 
            let output = func input
            printf "%A\n" output
            if count = 1 then
                () // output
            else 
                inner output (count-1)
            // let ouput = func input
        inner input n


         
    iter tick s 3

    // let y = tick s
    // let z = tick y

    // printf "%A\n\n" s
    // print "------------------------------------------------------"
    // printf "%A\n\n" y
    // print "------------------------------------------------------"

    // printf "%A\n\n" z
    
        




    0