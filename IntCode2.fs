// INTCODE VERSION 2

module IntCode2

    open System
    open Utils

    let getOpCode op =
        let s = op.ToString()
        if String.length s > 2 then
            s.[(String.length s - 2)..] |> Int64.Parse
        else op

    let resolve (opcode: Int64[]) (prog: Int64[]) rb =
        
        let op = getOpCode opcode.[0] 

        if op = 99L then
             [| op; opcode.[1]; 0L; 0L |]

        else 
            let def = if op = 3L then '1' else '0'
            let l = opcode.[0].ToString()
            let ops = l.PadLeft(5, def)
            let modes =  ops |> Seq.toArray |> Array.rev |> Array.skip 2

            // printf "Modes : %A\n" modes
            // let p1 = if modes.[0] = '0' then prog.[opcode.[1]] else opcode.[1]

            let p1 = 
                match modes.[0] with
                | '0' -> prog.[int(opcode.[1])]
                | '1' -> opcode.[1]
                | '2' -> if op = 3L then // || op = 4L then
                            int64(int(rb) + int(opcode.[1]))
                         else
                            prog.[int(rb) + int(opcode.[1])]
                | _ -> failwith "Invalid pointer"


            //printf "rb   : %A\n" rb
            //printf "indx : %A\n" (int(rb) + int(opcode.[1])) 
            //printf "prog : %A\n" prog
            //printf "Opcode is : %A\n" op

            let p2 = 
                if op <> 3L && op <> 4L && op <> 9L then
                    match modes.[1] with
                    | '0' -> prog.[int(opcode.[2])]
                    | '1' -> opcode.[2]
                    | '2' -> prog.[int(rb) + int(opcode.[2])]
                    | _ -> failwith "Invalid opcode 2"
                else
                    0L

            let p3 = 
                if op = 1L || op = 2L || op = 7L || op = 8L then
                    match modes.[2] with
                    | '0' -> opcode.[3]
                    | '2' -> opcode.[3] + int64(rb)
                    | _ -> failwith "Invalid opcode 3"
                else
                    0L


            [| op; p1; p2; p3 |]
//1008

    // Input  --   inst ptr      --  prog
    // Output --   new inst ptr  --  updated prog  --  last run op
    let tick ptr prog input rb = 
        let debug = true

        let opcode = prog |> Array.skip ptr |> Array.truncate 4
        printf "\ninst:  %A\n" opcode
        let resolved = resolve opcode prog rb
        printf "resv:  %A\n" resolved

        let op = 
            match resolved with // all ops here are immediate
            | [| 1L; x; y; z |] -> Array.set prog (int z) (x + y)
                                   [|1L; x; y; z|], ptr + 4 //Add
            | [| 2L; x; y; z |] -> Array.set prog (int z) (x * y)
                                   [|2L; x; y; z|], ptr + 4 //Mul
            | [| 3L; x; _; _ |] -> if debug then printf "write %A to %A\n" input x 
                                   Array.set prog (int x) input
                                   [|3L; x|], ptr + 2       //Input
            | [| 4L; x; _; _ |] -> if debug then printf "**********\n"
                                   if debug then printf "output %A\n" x
                                   if debug then printf "**********\n"
                                   [|4L; x|], ptr + 2       //Output

            | [| 5L; x; y; _ |] -> if debug then printf "Jump to %A  if (%A <> 0L)\n" y x 
                                   [| 5L; x; y|], if x <> 0L then (int y) else ptr + 3  // JNZ
                                    
            | [| 6L; x; y; _ |] -> if debug then printf "Jump to %A  if (%A = 0L)\n" y x 
                                   [| 6L; x; y|], if x = 0L then (int y) else ptr + 3  // JEZ

            | [| 7L; x; y; z |] -> if debug then printf "Set %A to 1 if (%A < %A) else 0\n" z x y
                                   Array.set prog (int z) (if x < y then 1L else 0L)
                                   if debug then printf "%A is %A\n" z prog.[(int z)]
                                   [|7L; x; y; z|], ptr + 4 //Lt

            | [| 8L; x; y; z |] -> if debug then printf "Set %A to 1 if (%A = %A) else 0\n" z x y
                                   Array.set prog (int z) (if x = y then 1L else 0L)
                                   [|8L; x; y; z|], ptr + 4 //Eq

            | [| 9L; x; _; _ |] -> if debug then printf "Add %A to relative \n" x  
                                   [|9L; x|], ptr + 2 // Relative

            | [| 99L; x; _; _|] -> [|99L; x|], 0
                                  
            | _ -> [||], 0
        
        snd op, prog, op
    
   

    let initialise (progin: Int64[]) inFunc =
        let mutable ptr = 0
       // let mutable inputPtr = 0
        let mutable cond = true
        let mutable out = 0
        let mutable rb = 0
        
        // temp
        let prog = Array.append progin (Array.create 10000 0L)
       
        while cond do
            // print ""

            print "Waiting for input ..."
            let (inp:int64) = inFunc() 
            printf "Got intput %A\n" inp

            // let inp = if Array.length inputs > inputPtr then  // dont need both of these
            //                 inputs.[inputPtr]
            //            else 
            //                 0L

            let inpxx = 0L
            let nptr, prog, op = tick ptr prog inpxx rb

            
            printf "fst op : %A\n" (fst op)

           // if (fst op).[0] = 3L then
           //     inputPtr <- (inputPtr + 1) % Array.length inputs

            if (fst op).[0] = 4L then
                out <- int (fst op).[1]
            
            if (fst op).[0] = 9L then
                // printf "Relavitve : %A\n" (fst op).[1]
                rb <- rb + int (fst op).[1]
                printf "Relative is : %A\n" rb

            if (fst op).[0] = 99L then // END
                cond <- false
              
        //     //let t = System.Console.ReadKey()
        //     ptr <- nptr


        printf "************** exec complete **************"
        //let t' = System.Console.ReadKey()
        out