module IntCode

    open System
    open Utils

    let getOpCode op =
        let s = op.ToString()
        if String.length s > 2 then
            s.[2..] |> Int64.Parse
        else op

    let resolve (opcode: Int64[]) (prog: Int64[]) rb =
        
        let op = getOpCode opcode.[0] 

        if op = 99L then
             [| op; opcode.[1]; 0L; 0L |]

        else 
            let def = if op = 3L then '1' else '0'
            let l = opcode.[0].ToString()

            let ops = l.PadLeft(4, def)
            let modes =  ops |> Seq.toArray |> Array.rev |> Array.skip 2


            // let p1 = if modes.[0] = '0' then prog.[opcode.[1]] else opcode.[1]

            let p1 = 
                match modes.[0] with
                | '0' -> prog.[int(opcode.[1])]
                | '1' -> opcode.[1]
                | '2' -> prog.[int(rb) + int(opcode.[1])]
                | _ -> failwith "Invalid opcode"

            printf "p1 : %A\n" prog

            let p2 = 
                if op <> 3L && op <> 4L && op <> 9L then
                    match modes.[1] with
                    | '0' -> prog.[int(opcode.[2])]
                    | '1' -> opcode.[2]
                    | '2' -> prog.[int(rb) + int(opcode.[1])]
                    | _ -> failwith "Invalid opcode"
                else
                    0L


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
        // 
        
        printf "resv:  %A\n" resolved

        let op = 
            match resolved with // all ops here are immediate
            | [| 1L; x; y; z |] -> Array.set prog (int z) (x + y)
                                   [|1L; x; y; z|], ptr + 4 //Add
            | [| 2L; x; y; z |] -> Array.set prog (int z) (x * y)
                                   [|2L; x; y; z|], ptr + 4 //Mul
            | [| 3L; x; _; _ |] -> Array.set prog (int x) input
                                   printf "write %A to %A\n" input x
                                   [|3L; x|], ptr + 2       //In
            | [| 4L; x; _; _ |] -> printf " ==========%A\n" x
                                   [|4L; x|], ptr + 2       //Out

            | [| 5L; x; y; _ |] -> printf "Jump to %A  (%A <> 0)\n" y x 
                                   [| 5L; x; y|], if x <> 0L then (int y) else ptr + 3  // JNZ
                                    
            | [| 6L; x; y; _ |] -> [| 6L; x; y|], if x = 0L then (int y) else ptr + 3  // JEZ

            | [| 7L; x; y; z |] -> Array.set prog (int z) (if x < y then 1L else 0L)
                                   [|7L; x; y; z|], ptr + 4 //Lt

            | [| 8L; x; y; z |] -> Array.set prog (int z) (if x = y then 1L else 0L)
                                   [|8L; x; y; z|], ptr + 4 //Eq

            | [| 9L; x; _; _ |] -> [|9L; x|], ptr + 2 // Relative

            | [| 99L; x; _; _|] -> [|99L; x|], 0
                                  
            | _ -> [||], 0
        
        snd op, prog, op
    

    let execute (progin: Int64[]) (inputs: int64[]) =
        let mutable ptr = 0
        let mutable inputPtr = 0
        let mutable cond = true
        let mutable out = 0
        let mutable rb = 0
        
        // temp
        let prog = Array.append progin (Array.create 100 0L)
       
        while cond do
            // print ""
            let inp = if Array.length inputs > inputPtr then
                            inputs.[inputPtr]
                        else 
                            0L

            let nptr, prog, op = tick ptr prog inp rb

            if (fst op).[0] = 3L then
                inputPtr <- (inputPtr + 1) % Array.length inputs

            if (fst op).[0] = 4L then
                out <- int (fst op).[1]
            
            if (fst op).[0] = 9L then
                // printf "Relavitve : %A\n" (fst op).[1]
                rb <- rb + int (fst op).[1]

            if (fst op).[0] = 99L then // END
                cond <- false
              
            let t = System.Console.ReadKey()
            ptr <- nptr


        printf "************** exec complete: %A\n" out
        let t' = System.Console.ReadKey()
        out
