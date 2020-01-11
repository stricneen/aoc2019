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

    let sendOutput (msg: Message) =
        printf "Sending : %A\n" msg

        // let command = msg / 1000L
        // let address =
        //     if command > 0L then
        //         int(msg - command * 1000L)
        //     else
        //         1000 - int(command * 1000L - msg)

        let reciept = network |> List.tryFind(fun x' -> let x,q,c = x'
                                                        x = msg.address )

        match reciept with
        | None -> printf "Can't find address : %A\n" msg.address
                  finished <- true
        | Some (_,y,_) -> y.Enqueue msg.x
                          y.Enqueue msg.y

        ()


    network
    |> List.iteri(fun i x' ->
        let x, q, c = x'
        printn i
        q.Enqueue -1L
        q.Enqueue -1L
        q.Enqueue -1L
        q.Enqueue -1L
        q.Enqueue -1L
        q.Enqueue -1L
        q.Enqueue -1L


        c.Initialise (Array.copy prog) q sendOutput
        |> Async.Start

    )


    while not finished do

        // print "press key"
        // System.Console.ReadKey()
        // network
        // |> List.iteri(fun i x' ->
        //     let x, q, c, e = x'
        //     e.Set()
        //     ()
        // )

        async { do! Async.Sleep(5000) } |> ignore


    0