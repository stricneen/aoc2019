module Day2

open Utils

let day2 =

    // let replace arr ind va =
    //    arr 
    //    |> Array.map (fun (x, i) -> if i = ind then x else va)



    print "Advent of code - Day 2"

    let prog = readCSV "./data/day2.txt"

    printfn "Out: %A" prog   

    // Fix
    Array.set prog 1 12
    Array.set prog 2 2


    printfn "Out: %A" prog   
    

    // let prog2 = [| 2; 0; 0; 0; 99 |]
    // let prog3 = [|  1;1;1;4;99;5;6;0;99 |]
             

    let tick inst = match inst with
                            | [| 1; x; y; z |] -> Array.set prog z (prog.[x] + prog.[y])
                                                  
                                                 // printfn "Add: %A" inst
                                                 // printfn "Add: %A" prog
                                                  prog
                            | [| 2; x; y; z |] -> Array.set prog z (prog.[x] * prog.[y])
                                                 // printfn "Mul: %A" inst
                                                  //printfn "Mul: %A" prog
                                                  prog
                            | _ -> prog

    let instCount = prog |> Array.chunkBySize 4 |> Array.length
    
    printfn "Chunks: %A" instCount


    let x = for var = 0 to instCount - 1 do
                let inst = prog |> Array.chunkBySize 4 |> Array.skip var
                let t = tick inst.[0]
                printfn "Chunk: %A" inst
                printfn "Prog: %A" prog
                

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
