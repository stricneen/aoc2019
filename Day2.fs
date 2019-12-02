module Day2

open Utils

let day2 =

    let replace arr ind va =
       arr 
       |> Array.map (fun (x, i) -> if i = ind then x else va)



    print "Advent of code - Day 2"

    let input = readCSV "./data/day1.txt"

    let prog2 = [| 2; 0; 0; 0; 99 |]

    let prog = [|  1;1;1;4;99;5;6;0;99 |]
             

    let tick inst = match inst with
                    | [| 1; x; y; z |] -> Array.set prog z (prog.[x] + prog.[y])
                                          printfn "Add: %A" inst
                                          printfn "Add: %A" prog
                                          prog
                    | [| 2; x; y; z |] -> Array.set prog z (prog.[x] * prog.[y])
                                          printfn "Mul: %A" inst
                                          printfn "Mul: %A" prog
                                          prog
                    | _ -> prog

    let out = prog
                |> Array.chunkBySize 4
                
                
   




    printfn "%A" out


    // match inst.Head with
    // | [| 1, x, y, z |] -> ()

   


    ()
