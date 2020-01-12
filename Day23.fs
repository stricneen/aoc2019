module Day23

open Utils
open IntCode3



let day23 =
    print "Advent of code - Day 23 - Category Six"


    let prog = readCSV "./data/day23.txt"

    let mutable finished = false



    let network = List.init 50 (fun x ->

        let q = System.Collections.Generic.Queue<int64>()
        let c = IntCode3 (x.ToString())
        (x, q, c)
    )

    let mutable nat = { address = 0; x = 0L; y = 0L }
    
    let comp address =
        network |> List.tryFind(fun x' -> let x,q,c = x'
                                          x = address )

//http://23.102.48.255:9000/api/flextool


//51.140.59.213 (oad-devtest-sit-flextool-appgateway.uksouth.cloudapp.azure.com)



    let mutable yvs = 0L

    let sendOutput (msg: Message) =

        let reciept = comp msg.address
        match reciept with
        | None -> //printf "NAT : %A\n" msg.address
                  if msg.address = 255 then
                        nat <- msg
                        
                        // printf "NAT  : %A\n" msg
                        // System.Console.ReadKey() |> ignore

                        // if yvs = msg.y then
                        //     printf "found : %A\n" msg
                        //     finished <- true
                        // else
                        //     yvs <- msg.y
                            
                            // let zero = comp 0
                            // let _,y,_ = zero.Value
                            // y.Enqueue msg.x
                            // y.Enqueue msg.y


        | Some (_,y,_) -> y.Enqueue msg.x
                          y.Enqueue msg.y

        ()


    let network' = network
                    |> List.mapi(fun i x' ->
                        let x, q, c = x'
                        printn i
                        q.Enqueue -1L
      
                        let tick = c.Initialise (Array.copy prog) q sendOutput
                        
                        x, q, c, tick
                    )



    while not finished do
    
        let t = network'
                |> List.fold (fun acc x' ->
                
                      let x, q, c, tick = x'
                      let qc, po = tick()
                    //   if qc > 0 then
                    //     printn qc
                      qc + fst acc, snd acc || po
                ) (0, false)


        if fst t = 0 && snd t then
            printf "send nat %A\n" nat.y
            let zero = comp 0
            let _,y,_ = zero.Value
            y.Enqueue nat.x
            y.Enqueue nat.y

            if yvs = nat.y && nat.y > 0L then
                printf "found : %A\n" nat
                finished <- true
            else
                yvs <- nat.y

        ()
        

     
        // System.Console.ReadKey()
       // async { do! Async.Sleep(5000) } |> ignore


    0


//     NAT  : { address = 255
//   x = 71153L
//   y = 17605L }


// found : { address = 255
//   x = 71153L
//   y = 18549L }

/// 16152 & 16153
/// 
/// // 16151