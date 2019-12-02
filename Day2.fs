module Day2

open Utils

let day2 =

    print "Advent of code - Day 2"

    let prog = readCSV "./data/day2.txt"

    // Apply fix
    Array.set prog 1 12
    Array.set prog 2 2

    let tick inst = match inst with
                            | [| 1; x; y; z |] -> Array.set prog z (prog.[x] + prog.[y])
                                                  prog
                            | [| 2; x; y; z |] -> Array.set prog z (prog.[x] * prog.[y])
                                                  prog
                            | _ -> prog

    let instCount = prog |> Array.chunkBySize 4 |> Array.length
    
    printfn "Chunks: %A" instCount


    for var = 0 to instCount - 1 do
        let inst = prog |> Array.chunkBySize 4 |> Array.skip var
        let t = tick inst.[0]
        ()
        // printfn "Chunk: %A" inst
        // printfn "Prog: %A" prog
            

    printfn ""
    printfn "Out: %A" prog   










    // let inst = prog
    //             |> Array.chunkBySize 4
    //             |> Array.skip x
    //             |> Array.take 1
    
                
                
    




    //printfn "%A" out


    // match inst.Head with
    // | [| 1, x, y, z |] -> ()

   


    ()
