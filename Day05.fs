module Day5

open Utils
open System

let day5 = 
    print "Advent of code - Day 5 - Sunny with a Chance of Asteroids"
    
    let prog = readCSV "./data/day5.txt"

    let input = 1

    let getOpCode op =
        let s = op.ToString()
        if String.length s > 2 then
            s.[2..] |> Int32.Parse
        else op

    // 4 array - will resolve to immediate
    let resolve (opcode: int[]) prog =
        // what is the op
        let op = getOpCode opcode.[0] // 1001
        printf "opcode : %A\n" op

        Array.append [|op|] opcode.[1..]


    // Input  --   inst ptr        prog
    // Output --   new inst ptr    updated prog    last run op
    let tick ptr prog = 
        let opcode = prog |> Array.skip ptr |> Array.take 4
        printf "inst:  %A\n" opcode
        let resolved = resolve opcode prog
        let op = 
            match resolved with
            | [| 1; x; y; z |] -> Array.set prog z (prog.[x] + prog.[y])
                                  [|1; x; y; z|] //Add
            | [| 2; x; y; z |] -> Array.set prog z (prog.[x] * prog.[y])
                                  [|2; x; y; z|] //Mul
            | [| 3; x; _; _ |] -> Array.set prog x input
                                  [|3; x|]       //In
            | [| 4; x; _; _ |] -> printf "%A\n" prog.[x]
                                  [|4; x|]       //Out
            | [| 99; _; _; _|] -> [|99|]
            | _ -> [||]
        
        ptr + Array.length op, prog, op
    
    let mutable ptr = 0
    while true do
        let x,y,z = tick ptr prog

        print ""
        let t = System.Console.ReadKey()

        ptr <- x




    
    0
    


//    // let decom prog = 



//     let extract prog = 
//         match Seq.head prog with
//               | 1 -> Seq.take 4 prog, prog |> Seq.skip 4
//               | 2 -> Seq.take 4 prog, prog |> Seq.skip 4
//               | 3 -> Seq.take 2 prog, prog |> Seq.skip 2
//               | 4 -> Seq.take 2 prog, prog |> Seq.skip 2
//               | _ -> Seq.empty, Seq.empty
    

        


//    // let codes = opcodes prog
//     //let two = op (snd one)

//    // printf "%A\n" (codes |> Seq.toList)

//     //printf "%A\n" one
//     //printf "%A\n" two

    
  

//     // let execute prog noun verb =     
    
//     //     Array.set prog 1 noun
//     //     Array.set prog 2 verb

//     //     let tick inst = match inst with
//     //                             | [| 1; x; y; z |] -> Array.set prog z (prog.[x] + prog.[y])
//     //                                                   prog
//     //                             | [| 2; x; y; z |] -> Array.set prog z (prog.[x] * prog.[y])
//     //                                                   prog
//     //                             | _ -> prog

//     //     let instCount = prog |> Array.chunkBySize 4 |> Array.length
        
//     //     // printfn "Chunks: %A" instCount

//     //     for var = 0 to instCount - 1 do
//     //         let inst = prog |> Array.chunkBySize 4 |> Array.skip var
//     //         let t = tick inst.[0]
//     //         ()
    
//     //     prog.[0]




     






        