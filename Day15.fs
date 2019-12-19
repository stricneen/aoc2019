module Day15

open Utils
open IntCode2

type Direction = North = 1L | East = 4L | South = 2L | West = 3L | End = 4L

let day15 = 
    print "Advent of code - Day 15 - Oxygen System"

    let prog = readCSV "./data/day15.txt" 

    let comp = IntCode2("maze")
    let inq = comp.Initialise prog
    let mutable finished = false

    let printStatus msg = 
        printAt 1 1 msg

    let mutable startpos = 50, 25
    let mutable endpos = 50, 25
    let mutable location = 50, 25
    let mutable dir = Direction.North
    let wall = "#"
    let corridor = "."
    inq.Post (int64 dir)

    comp.OutputReady.Add(fun output ->
          if output = -99999L then
              finished <- true
          else 
              printAt (fst location) (snd location) "D"
              dir <-
                  match output, location with
                  | 0L, (x,y) when dir =  Direction.North -> printStatus "Can't go N - trying E"
                                                             printAt x (y-1) wall
                                                             Direction.East
                  | 0L, (x,y) when dir =  Direction.East -> printStatus "Can't go E - trying S"
                                                            printAt (x+1) y wall
                                                            Direction.South
                  | 0L, (x,y) when dir =  Direction.South -> printStatus "Can't go S - trying W"
                                                             printAt x (y+1) wall
                                                             Direction.West
                  | 0L, (x,y) when dir =  Direction.West -> printStatus "Can't go W - trying N"
                                                            printAt (x-1) y wall
                                                            Direction.North
                  | 1L, (x,y) when dir =  Direction.North -> printStatus "Going N     "
                                                             printAt x y corridor
                                                             location <- (x, y-1)
                                                             Direction.West
                  | 1L, (x,y) when dir =  Direction.East -> printStatus "Going E      "
                                                            printAt x y corridor
                                                            location <- (x+1, y)
                                                            Direction.North
                  | 1L, (x,y) when dir =  Direction.South -> printStatus "Going S      "
                                                             printAt x y corridor
                                                             location <- (x, y+1)
                                                             Direction.East
                  | 1L, (x,y) when dir =  Direction.West -> printStatus "Going W        "
                                                            printAt x y corridor
                                                            location <- (x-1, y)
                                                            Direction.South

                  | 2L, (x,y) when dir =  Direction.North -> printStatus "Going N     "
                                                             printAt x y corridor
                                                             endpos <- location
                                                             location <- (x, y-1)
                                                             Direction.West
                  | 2L, (x,y) when dir =  Direction.East -> printStatus "Going E      "
                                                            printAt x y corridor
                                                            endpos <- location
                                                            location <- (x+1, y)
                                                            Direction.North
                  | 2L, (x,y) when dir =  Direction.South -> printStatus "Going S      "
                                                             printAt x y corridor
                                                             endpos <- location
                                                             location <- (x, y+1)
                                                             Direction.East
                  | 2L, (x,y) when dir =  Direction.West -> printStatus "Going W        "
                                                            printAt x y corridor
                                                            endpos <- location
                                                            location <- (x-1, y)
                                                            Direction.South
                //   | 2L, (_,_) -> printAt 10 80  "************************DONE********************"
                //                  endpos <- location
                //                  Direction.South
                //                  //finished <- true
                //                  //Direction.End
                  | _ -> failwith "invalid user"


              printAt (fst endpos) (snd endpos) "F"
              printAt (fst startpos) (snd startpos) "S"

              printAt (fst location) (snd location) "D"
              // System.Console.ReadKey() 
              
              //if dir <> Direction.End then
              inq.Post (int64 dir)

         )
        
    while not finished do
        async {
            do! Async.Sleep(100) } |> ignore




    0