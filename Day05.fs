module Day5

// open Utils
// open System

// let day5 = 
//     print "Advent of code - Day 5 - Sunny with a Chance of Asteroids"
    
//     let prog = readCSV "./data/day5.txt"

//     let input = 5

//     let getOpCode op =
//         let s = op.ToString()
//         if String.length s > 2 then
//             s.[2..] |> Int32.Parse
//         else op

//     // 4 array - will resolve to immediate
//     let resolve (opcode: int[]) (prog: int[]) =
//         let op = getOpCode opcode.[0] 
//         printf "opcode : %A\n" op

// // [|1; 225; 6; 6|]

//         let def = if op = 3 then '1' else '0'
//         let l = opcode.[0].ToString()

//         let ops = l.PadLeft(4, def)
  
//         //printf "ops : %A\n" ops
        
//         let modes =  ops |> Seq.toArray |> Array.rev |> Array.skip 2
               
//         printf "modes : %A\n" modes

//         let p1 = if modes.[0] = '0' then prog.[opcode.[1]] else opcode.[1]
//         printf "p1 : %A\n" p1
        

//         let p2 = 
//             if op < 3 || op > 4 then
//                 if modes.[1] = '0' then prog.[opcode.[2]] else opcode.[2]
//             else
//                 0
//         printf "p2 : %A\n" p2

//         [| op; p1; p2; opcode.[3] |]


//     // Input  --   inst ptr      --  prog
//     // Output --   new inst ptr  --  updated prog  --  last run op
//     let tick ptr prog = 
//         let opcode = prog |> Array.skip ptr |> Array.take 4
//         printf "inst:  %A\n" opcode
//         let resolved = resolve opcode prog
//         let op = 
//             match resolved with // all ops here are immediate
//             | [| 1; x; y; z |] -> Array.set prog z (x + y)
//                                   [|1; x; y; z|], ptr + 4 //Add
//             | [| 2; x; y; z |] -> Array.set prog z (x * y)
//                                   [|2; x; y; z|], ptr + 4 //Mul
//             | [| 3; x; _; _ |] -> Array.set prog x input
//                                   printf "write %A to %A" input x
//                                   [|3; x|], ptr + 2       //In
//             | [| 4; x; _; _ |] -> print "================"
//                                   printf "%A\n" x
//                                   print "================"
//                                   [|4; x|], ptr + 2       //Out
            
//             | [| 5; x; y; _ |] -> if x <> 0 then
//                                     [| 5; x; y|], y
//                                   else 
//                                     [| 5; x; y|], ptr + 3
                                 
//             | [| 6; x; y; _ |] -> if x = 0 then
//                                     [| 6; x; y|], y
//                                   else 
//                                     [| 6; x; y|], ptr + 3

//             | [| 7; x; y; z |] -> Array.set prog z (if x < y then 1 else 0)
//                                   [|7; x; y; z|], ptr + 4 //Lt

//             | [| 8; x; y; z |] -> Array.set prog z (if x = y then 1 else 0)
//                                   [|8; x; y; z|], ptr + 4 //Lt

//             | [| 99; _; _; _|] -> [|99|], 0
//             | _ -> [||], 0
        
//         snd op, prog, op
    
//     let mutable ptr = 0
//     while true do
//         let x,y,z = tick ptr prog

//         print ""
//         let t = System.Console.ReadKey()

//         ptr <- x




    
//     0
    


// //    // let decom prog = 



// //     let extract prog = 
// //         match Seq.head prog with
// //               | 1 -> Seq.take 4 prog, prog |> Seq.skip 4
// //               | 2 -> Seq.take 4 prog, prog |> Seq.skip 4
// //               | 3 -> Seq.take 2 prog, prog |> Seq.skip 2
// //               | 4 -> Seq.take 2 prog, prog |> Seq.skip 2
// //               | _ -> Seq.empty, Seq.empty
    

        


// //    // let codes = opcodes prog
// //     //let two = op (snd one)

// //    // printf "%A\n" (codes |> Seq.toList)

// //     //printf "%A\n" one
// //     //printf "%A\n" two

    
  

// //     // let execute prog noun verb =     
    
// //     //     Array.set prog 1 noun
// //     //     Array.set prog 2 verb

// //     //     let tick inst = match inst with
// //     //                             | [| 1; x; y; z |] -> Array.set prog z (prog.[x] + prog.[y])
// //     //                                                   prog
// //     //                             | [| 2; x; y; z |] -> Array.set prog z (prog.[x] * prog.[y])
// //     //                                                   prog
// //     //                             | _ -> prog

// //     //     let instCount = prog |> Array.chunkBySize 4 |> Array.length
        
// //     //     // printfn "Chunks: %A" instCount

// //     //     for var = 0 to instCount - 1 do
// //     //         let inst = prog |> Array.chunkBySize 4 |> Array.skip var
// //     //         let t = tick inst.[0]
// //     //         ()
    
// //     //     prog.[0]




     






        