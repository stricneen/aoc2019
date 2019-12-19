module Day15

open Utils
open IntCode2

type Direction = North = 1L | East = 4L | South = 2L | West = 3L

let day15 = 
    print "Advent of code - Day 15 - Oxygen System"

    let prog = readCSV "./data/day15.txt" 

    let comp = IntCode2("maze")
    let inq = comp.Initialise prog
    let mutable finished = false

    let mutable location = 80, 50
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
                  | 0L, (x,y) when dir =  Direction.North -> printAt 0 0 "Can't go N - trying E"
                                                             printAt x (y-1) wall
                                                             Direction.East
                  | 0L, (x,y) when dir =  Direction.East -> printAt 0 0 "Can't go E - trying S"
                                                            printAt (x+1) y wall
                                                            Direction.South
                  | 0L, (x,y) when dir =  Direction.South -> printAt 0 0 "Can't go S - trying W"
                                                             printAt x (y+1) wall
                                                             Direction.West
                  | 0L, (x,y) when dir =  Direction.West -> printAt 0 0 "Can't go W - trying N"
                                                            printAt (x+1) y wall
                                                            Direction.North
                  | 1L, (x,y) when dir =  Direction.North -> printAt 0 0 "Going N"
                                                             printAt x y corridor
                                                             location <- (x, y-1)
                                                             Direction.West
                  | 1L, (x,y) when dir =  Direction.East -> printAt 0 0 "Going E"
                                                            printAt x y corridor
                                                            location <- (x+1, y)
                                                            Direction.North
                  | 1L, (x,y) when dir =  Direction.South -> printAt 0 0 "Going S"
                                                             printAt x y corridor
                                                             location <- (x, y+1)
                                                             Direction.East
                  | 1L, (x,y) when dir =  Direction.West -> printAt 0 0 "Going W"
                                                            printAt x y corridor
                                                            location <- (x-1, y)
                                                            Direction.South
                  | _ -> failwith "invalid user"
               
              inq.Post (int64 dir)

         )
        
    while not finished do
        async {
            do! Async.Sleep(100) } |> ignore




    0