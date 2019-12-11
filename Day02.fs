module Day2

// open Utils

// // Op codes

// let day2 =

//     print "Advent of code - Day 2 - 1202 Program Alarm"

//     let execute prog noun verb =     
    
//         Array.set prog 1 noun
//         Array.set prog 2 verb

//         let tick inst = match inst with
//                                 | [| 1; x; y; z |] -> Array.set prog z (prog.[x] + prog.[y])
//                                                       prog
//                                 | [| 2; x; y; z |] -> Array.set prog z (prog.[x] * prog.[y])
//                                                       prog
//                                 | _ -> prog

//         let instCount = prog |> Array.chunkBySize 4 |> Array.length
        
//         // printfn "Chunks: %A" instCount

//         for var = 0 to instCount - 1 do
//             let inst = prog |> Array.chunkBySize 4 |> Array.skip var
//             let t = tick inst.[0]
//             ()
    
//         prog.[0]


//     let prog = readCSV "./data/day2.txt"

//     let f = execute prog 12 2

//     printfn ""
//     printfn "Out: %A" f   


//     print "Part 2"

//     for noun = 0 to 99 do
//         for verb = 0 to 99 do
//             let prog = readCSV "./data/day2.txt"
//             let f = execute prog noun verb
//             if f = 19690720 then    
//                 printn noun
//                 printn verb
//                 printn ((100 * noun) + verb)




//     ()
