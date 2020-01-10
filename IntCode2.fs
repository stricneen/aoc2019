// INTCODE VERSION 2

module IntCode2

    open System
    open Utils

    type IntCode2(name) = 
    
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
            let debug = false

            let opcode = prog |> Array.skip ptr |> Array.truncate 4
            //printf "inst:  %A\n" opcode
            let resolved = resolve opcode prog rb
            if debug then printf "(%A) inst:  %A\t\t    resv:  %A\n" name opcode resolved
         
            let op = 
                match resolved with // all ops here are immediate
                | [| 1L; x; y; z |] -> Array.set prog (int z) (x + y)
                                       [|1L; x; y; z|], ptr + 4 //Add
                | [| 2L; x; y; z |] -> Array.set prog (int z) (x * y)
                                       [|2L; x; y; z|], ptr + 4 //Mul
                | [| 3L; x; _; _ |] -> if input <> -1L then
                                            printf "(%A) write %A to %A\n" name input x 
                                       Array.set prog (int x) input
                                       [|3L; x|], ptr + 2       //Input
                | [| 4L; x; _; _ |] -> if debug then printf "output %A\n" x
                                       [|4L; x|], ptr + 2       //Output
                                       
                | [| 5L; x; y; _ |] -> if debug then printf "Jump to %A if (%A <> 0L)\n" y x 
                                       [| 5L; x; y|], if x <> 0L then (int y) else ptr + 3  // JNZ
                                        
                | [| 6L; x; y; _ |] -> if debug then printf "Jump to %A if (%A = 0L)\n" y x 
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

                | [| 99L; x; _; _|] -> if debug then print "99 - all done \n"
                                       
                                       [|99L; x|], 0
                                      
                | _ -> [||], 0
      
            snd op, prog, op
       
        let outputEvent = new Event<int64>()
       
        let mutable arevent:Threading.AutoResetEvent = null
        let mutable tag = 0L,0L
        //member this.SetTag<T> (tag:T)= 

        
       // let mutable booted = false
       // let mutable step = true
        member this.OutputReady = outputEvent.Publish

        // member this.Booted b = 
        //     booted <- b
        member this.AutoReEvent are =
            arevent <- are


       // member this.Step = step <- false

        member this.Initialise (progin: Int64[]) =
            let mutable ptr = 2

            Array.set progin 62 (Int64.Parse name)

            // let mutable inputPtr = 0
            // let mutable cond = true
            // let mutable out = 0
            let mutable rb = 0
            let mutable input = 0L
           
            let mutable outstate = 0 // 0 - address : 1 - x : 2 - y
            let mutable address = 0L
            let mutable xout = 0L
            let mutable yout = 0L
            
            let mutable f = true
            let mutable first = true

            let mutable cx = 0

            // temp
            let prog = Array.append progin (Array.create 10000 0L)

            let inputQueue = MailboxProcessor<int64>.Start(fun inbox ->

                let rec messageLoop() = async {
                   // printf "(%A) Running op : %A\n" name prog.[ptr] 

                    let c = (prog.[ptr]).ToString()
                    let x = c.[(String.length c) - 1]


                    if x = '3' then
 //                       System.Console.ReadKey()
   //                     printf "waiting for %A\n" name
                        let! opt = inbox.TryReceive 5
                        let input' = match opt with
                                     | None -> if f then
                                                    print name
                                                    if cx = 1 then  
                                                        print "xxxxxxxx"
                                                        System.Console.ReadKey()
                                                        ()
                                                    if cx = 2 then  
                                                        print "999999"
                                                        System.Console.ReadKey()
                                                        ()
                                                    cx <- 0
                                                    f <- false
                                                    first <- false
                                                    -1L
                                               else 
                                                    -1L
                                     | Some message ->  cx <- cx + 1
                                                        print "bbbbbb"
                                                        System.Console.ReadKey()
                                                        message

                        input <- input'
      
                    let nptr, prog, op = tick ptr prog input rb
                 
                    if (fst op).[0] = 4L then     /// need to output
                        let outputValue = (fst op).[1]

                        if outstate = 0 then
                            //printf "(%A) Address ... %A\n" name outputValue
                            address <- outputValue
                            outstate <- 1
                        else if outstate = 1 then
                            // printf "(%A) X ... %A\n" name outputValue
                            xout <- outputValue * 1000L + address
                            outstate <- 2
                            // outputEvent.Trigger(outputValue * 1000L + address)
                            // printf "(%A) X SENT... %A\n" name outputValue
                            
                        else
                           // printf "ov: %A\n" outputValue
                            yout <- outputValue * 1000L + address
                            printf "(%A) to:%A  x:%A   y:%A\n" name address (xout/1000L) (yout/1000L)
                            
                            outputEvent.Trigger(xout)
                            outputEvent.Trigger(yout)
                            
                            // if address = 255L then
                            //     printf "ANSWER : %A\n" outputValue 
                            outstate <- 0
                        
                        // 66968L x
                        //out <- int (fst op).[1]
                    
                    if (fst op).[0] = 9L then
                        rb <- rb + int (fst op).[1]
                      
                    if (fst op).[0] = 99L then // END
                        outputEvent.Trigger(-99999L)
                        
                        // print "Exiting Intcode comp\n"
                        return ()

                    ptr <- nptr

                    printf "(%A) Waiting : %A\n" name

                   
                   // arevent.WaitOne() |> ignore

                    // while step do
                    //     Threading.Thread.Sleep 5000

                    //step <- true

                    return! messageLoop()
                }
                messageLoop() // start the loop
            )

            
            inputQueue
          
          
      