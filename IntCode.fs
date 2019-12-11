module IntCode

    open System
    open Utils

    let getOpCode op =
        let s = op.ToString()
        if String.length s > 2 then
            s.[2..] |> Int32.Parse
        else op

    let resolve (opcode: int[]) (prog: int[]) rb =
        
        let op = getOpCode opcode.[0] 

        if op = 99 then
             [| op; opcode.[1]; 0; 0 |]

        else 
            let def = if op = 3 then '1' else '0'
            let l = opcode.[0].ToString()

            let ops = l.PadLeft(4, def)
            let modes =  ops |> Seq.toArray |> Array.rev |> Array.skip 2


            // let p1 = if modes.[0] = '0' then prog.[opcode.[1]] else opcode.[1]

            let p1 = 
                match modes.[0] with
                | '0' -> prog.[opcode.[1]]
                | '1' -> opcode.[1]
                | '2' -> prog.[rb + opcode.[1]]
                | _ -> failwith "Invalid opcode"

            let p2 = 
                if op <> 3 && op <> 4 && op <> 9 then
                    match modes.[1] with
                    | '0' -> prog.[opcode.[2]]
                    | '1' -> opcode.[2]
                    | '2' -> prog.[rb + opcode.[2]]
                    | _ -> failwith "Invalid opcode"
                else
                    0


            // let p2 = 
            //     if op <> 3 && op <> 4 then
            //         if modes.[1] = '0' then prog.[opcode.[2]] else opcode.[2]
            //     else
            //         0

            [| op; p1; p2; opcode.[3] |]


    // Input  --   inst ptr      --  prog
    // Output --   new inst ptr  --  updated prog  --  last run op
    let tick ptr prog input rb = 
        let opcode = prog |> Array.skip ptr |> Array.truncate 4
        printf "inst:  %A\n" opcode
        let resolved = resolve opcode prog rb
        printf "resv:  %A\n" resolved

        let op = 
            match resolved with // all ops here are immediate
            | [| 1; x; y; z |] -> Array.set prog z (x + y)
                                  [|1; x; y; z|], ptr + 4 //Add
            | [| 2; x; y; z |] -> Array.set prog z (x * y)
                                  [|2; x; y; z|], ptr + 4 //Mul
            | [| 3; x; _; _ |] -> Array.set prog x input
                                  printf "write %A to %A\n" input x
                                  [|3; x|], ptr + 2       //In
            | [| 4; x; _; _ |] -> printf "==========%A\n" x
                                  [|4; x|], ptr + 2       //Out

            | [| 5; x; y; _ |] -> printf "Jump to %A  (%A <> 0)\n" y x 
                                  [| 5; x; y|], if x <> 0 then y else ptr + 3  // JNZ
                                    
            | [| 6; x; y; _ |] -> [| 6; x; y|], if x = 0 then y else ptr + 3  // JEZ

            | [| 7; x; y; z |] -> Array.set prog z (if x < y then 1 else 0)
                                  [|7; x; y; z|], ptr + 4 //Lt

            | [| 8; x; y; z |] -> Array.set prog z (if x = y then 1 else 0)
                                  [|8; x; y; z|], ptr + 4 //Eq

            | [| 9; x; _; _ |] -> [|9; x|], ptr + 2 // Relative

            | [| 99; x; _; _|] -> [|99; x|], 0
                                  
            | _ -> [||], 0
        
        snd op, prog, op
    

    let execute prog (inputs: int[]) =
        let mutable ptr = 0
        let mutable inputPtr = 0
        let mutable cond = true
        let mutable out = 0
        let mutable rb = 0
        
        while cond do
            // print ""
            let inp = if Array.length inputs > inputPtr then
                            inputs.[inputPtr]
                        else 
                            0

            let nptr, prog, op = tick ptr prog inp rb

            if (fst op).[0] = 3 then
                inputPtr <- (inputPtr + 1) % Array.length inputs

            if (fst op).[0] = 4 then
                out <- (fst op).[1]
            
            if (fst op).[0] = 9 then
                printf "Relavitve : %A\n" (fst op).[1]
                rb <- (fst op).[1]

            if (fst op).[0] = 99 then // END
                cond <- false
              
            //let t = System.Console.ReadKey()
            ptr <- nptr


        printf "************** exec complete: %A\n" out
        //let t' = System.Console.ReadKey()
        out
