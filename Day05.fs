module Day5

open Utils

let day5 = 
    print "Advent of code - Day 5"

    let prog = readCSV "./data/day5.txt"

    let decom prog = 
        


    let extract prog = 
        match Seq.head prog with
              | 1 -> Seq.take 4 prog, prog |> Seq.skip 4
              | 2 -> Seq.take 4 prog, prog |> Seq.skip 4
              | 3 -> Seq.take 2 prog, prog |> Seq.skip 2
              | 4 -> Seq.take 2 prog, prog |> Seq.skip 2
              | _ -> Seq.empty, Seq.empty
    

        


    let codes = opcodes prog
    //let two = op (snd one)

    printf "%A\n" (codes |> Seq.toList)

    //printf "%A\n" one
    //printf "%A\n" two

    
  

    // let execute prog noun verb =     
    
    //     Array.set prog 1 noun
    //     Array.set prog 2 verb

    //     let tick inst = match inst with
    //                             | [| 1; x; y; z |] -> Array.set prog z (prog.[x] + prog.[y])
    //                                                   prog
    //                             | [| 2; x; y; z |] -> Array.set prog z (prog.[x] * prog.[y])
    //                                                   prog
    //                             | _ -> prog

    //     let instCount = prog |> Array.chunkBySize 4 |> Array.length
        
    //     // printfn "Chunks: %A" instCount

    //     for var = 0 to instCount - 1 do
    //         let inst = prog |> Array.chunkBySize 4 |> Array.skip var
    //         let t = tick inst.[0]
    //         ()
    
    //     prog.[0]




    0







        