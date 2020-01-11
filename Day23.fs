module Day23

open Utils
open IntCode3



let day23 =
    print "Advent of code - Day 23 - Category Six"


    let prog = readCSV "./data/day23.txt"

    let mutable finished = false


    // will be xxxxxxxaaa xxx : command,  aaa : address
    // let outputNic x =
    //     printf "OUTPUT : %A\n" x

    let network = List.init 50 (fun x ->

        let q = System.Collections.Generic.Queue<int64>()
        let c = IntCode3 (x.ToString())
        //let event = new System.Threading.AutoResetEvent(false)
        //c.AutoReEvent event
        //printn x
        // q.Post (int64 x)
        // c.OutputReady.Add(outputNic)
        (x, q, c)
    )

    let mutable nat = { address = 0; x = 0L; y = 0L }
    
    let comp address =
        network |> List.tryFind(fun x' -> let x,q,c = x'
                                          x = address )

//http://23.102.48.255:9000/api/flextool


//51.140.59.213 (oad-devtest-sit-flextool-appgateway.uksouth.cloudapp.azure.com)




    let sendOutput (msg: Message) =

        let reciept = comp msg.address
        match reciept with
        | None -> //printf "NAT : %A\n" msg.address
                  if msg.address = 255 then
                        
                        printf "NAT  : %A\n" msg
                        System.Console.ReadKey() |> ignore

                        if nat.y = msg.y then
                            printf "found : %A\n" nat
                            finished <- true
                        else
                            let zero = comp 0
                            let _,y,_ = zero.Value
                            y.Enqueue msg.x
                            y.Enqueue msg.y


                        nat <- msg
        | Some (_,y,_) -> y.Enqueue msg.x
                          y.Enqueue msg.y

        ()


    let network' = network
                    |> List.mapi(fun i x' ->
                        let x, q, c = x'
                        printn i
                        q.Enqueue -1L
                        q.Enqueue -1L
                        q.Enqueue -1L
                        let tick = c.Initialise (Array.copy prog) q sendOutput
                        
                        x, q, c, tick
                    )




    while not finished do
    
        network'
        |> List.iter(fun x' ->
        
          let x, q, c, tick = x'
          tick()
        )
     
        // System.Console.ReadKey()
        async { do! Async.Sleep(5000) } |> ignore


    0