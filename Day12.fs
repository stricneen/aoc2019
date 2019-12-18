module Day12

open Utils

type Pos = { X:int;  }

let day12 = 
    print "Advent of code - Day 12 - The N-Body Problem"

// // <x=1, y=-4, z=3>
// // <x=-14, y=9, z=-4>
// // <x=-4, y=-6, z=7>
// // <x=6, y=-9, z=-11>


//     let io = { X=1; }
//     let europa = { X= -14; }
//     let ganymede = { X= -4; }
//     let callisto = { X=6;}
    
//     // let io = { X= -4; }
//     // let europa = { X= 9; }
//     // let ganymede = { X= -6; }
//     // let callisto = { X= -9;}

//     // let io = { X=3; }
//     // let europa = { X= -4; }
//     // let ganymede = { X= 7; }
//     // let callisto = { X= -11;}

//     let universe = [
//         io,         {X=0;};
//         europa,     {X=0;};
//         ganymede,   {X=0;};
//         callisto,   {X=0;};
//     ]

//     // Eg 1
//     // let a = { X= -1; }
//     // let b = { X=2;  }
//     // let c = { X=4;  }
//     // let d = { X=3; }

// // <x=-8, y=-10, z=0>
// // <x=5, y=5, z=10>
// // <x=2, y=-7, z=3>
// // <x=9, y=-8, z=-3>
    

//     // Eg 2
//     // let a = { X= -8; Y= -10; Z=0 }
//     // let b = { X=5; Y=5; Z=10 }
//     // let c = { X= 2; Y= -7; Z=3 }
//     // let d = { X= 9; Y= -8; Z= -3 }

//     // let s = [
//     //     a, {X=0;Y=0;Z=0};
//     //     b, {X=0;Y=0;Z=0};
//     //     c, {X=0;Y=0;Z=0};
//     //     d, {X=0;Y=0;Z=0};
//     // ]

//     // let pairs = comb 2 s

//     let n a b c =
//         if a < b then
//             1
//         else if a > b then
//            -1
//         else 
//             0
      

//     let applyVelocity (l: (Pos * Pos) list) =
//        // printf "A : %A\n\n" l
//        let a  = fst l.[0] 
//        let a' = snd l.[0]
//        let b  = fst l.[1]
//        let b' = snd l.[1]
//        [ 
//            (a.X * 1000 + a'.X, a, a', {X = n a.X b.X a'.X; }),
//            (b.X * 1000 + b'.X, b, b', {X = n b.X a.X b'.X; })
//        ]
//        // printf "B : %A\n\n" v
//        //v
        
//     let unwrapPairs acc ele =
//         let acc1 = fst ele :: acc
//         snd ele :: acc1

//     let sumVelocity acc (ele: int * Pos * Pos * Pos) = 
//         // printf "acc : %A\n\n" acc
//         // printf "ele : %A\n\n" ele
//         let i, pos, vel, del = ele

//         let x = acc |> List.where (fun x -> fst3 x = i)
//         if List.isEmpty x then 
//             (i, pos, { X = vel.X + del.X}) :: acc
//         else 
//             let nw = acc |> List.where (fun x -> fst3 x <> i)
//             let vel' = trd3 (List.head x)
//             (i, pos, { X = vel'.X + del.X }) :: nw

//     let applyGravity data =
//         let i, pos, vel = data
//         { X = pos.X + vel.X; }, vel

//     //for x in 1 .. 10 do 
//     let tick state =
//         comb 2 state
//                 |> List.map applyVelocity
//                 |> List.concat
//                 |> List.fold unwrapPairs []
//                 |> List.fold sumVelocity []
//                 |> List.map applyGravity

//     // let posValue pos = 
//     //     abs pos.X + abs pos.Y + abs pos.Z

//     // let totalEnergy (state: (Pos * Pos) list) =
//     //     state 
//     //     |> List.sumBy (fun x -> posValue (fst x) * posValue (snd x))
  
//     // let iter func (input: 't) n =    // move to utils
//     //     let rec inner (input: 't) count = 
//     //         let output = func input
//     //         //printf "%A\n" output
//     //         printf "Total : %A\n" (totalEnergy output)
//     //         if count = 1 then
//     //             () // output
//     //         else 
//     //             inner output (count - 1)
//     //         // let ouput = func input
//     //     inner input n


//     // iter tick s 1000

//     // // Part 2
//     // let posEquals pos1 pos2 =
//     //     pos1.X = pos2.X 

// //   try
// //         (x+1) / y                      // error here -- see below
// //     with
// //     | :? System.DivideByZeroException as ex -> 
// //           printfn "%s" ex.Message

//     let stateEquals s1 s2 =
     
//             s1
//             |> List.zip s2
//             |> List.forall (fun (x, y) -> (fst x).X = (fst y).X && (snd x).X = (snd y).X)
   
           

//     // let io =       { X= 1;   Y= -4;  Z= 3 }
//     // let europa =   { X= -14; Y= 9    Z= -4 }
//     // let ganymede = { X= -4;  Y= -6;  Z= 7 }
//     // let callisto = { X= 6;   Y= -9;  Z= -11 }
    
//     // let universe = [
//     //     io,         {X=0;Y=0;Z=0};
//     //     europa,     {X=0;Y=0;Z=0};
//     //     ganymede,   {X=0;Y=0;Z=0};
//     //     callisto,   {X=0;Y=0;Z=0};
//     // ]

// //    let hashX (state: (Pos * Pos) list) = 
// //        list |> 

//     let iter2 func (input: 't) (start:bigint) =    // move to utils

//         let initial = input
//    //     printf "%A\n" initial
//         let rec inner (input: 't) count = 
//             let output = func input
//          //   printf "A :: %A\n" output
//             if List.length output = 3 then 
//                 printf "A :: %A\n" input
//                 printf "B :: %A\n" output 
//                 failwith "bluergh"

//             if stateEquals initial output then
//                  printf "EQUAL: %A\n" count
//                  System.Console.ReadKey() |> ignore
                 

//             if count % 100000I = 0I then
//                 printf "%A\n" count
//                 printf "%A\n" output
//             //System.Console.ReadKey() |> ignore

//             //printf "Total : %A\n" (totalEnergy output)
//             // if count = 1 then
//             //     () // output
//             // else 
//             inner output (count + 1I)        
    
//         inner input start

//     //iter2 tick s 0
//     iter2 tick universe 0I

//     // let x:bigint = 161427I * 231613I * 116327I
//     // printf "output : %A"  x
//     0


//     // X = 161427
//     // Y = 231613
//     // Z = 116327   4349302712618577
//     //               543673227860472
    0