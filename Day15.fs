module Day15

open Utils
open IntCode2

type Direction = North = 1L | East = 4L | South = 2L | West = 3L | End = 4L

type Location = { x:int; y:int; pos:char; dist:int }

let day15 = 
    print "Advent of code - Day 15 - Oxygen System"

    let prog = readCSV "./data/day15.txt" 

    let coordsOf ary chr =
        let rec loop (a: char[,]) c =
            if c > Array2D.length1 ary - 1 then
                -1,-1
            else
                let row = a.[c, *]
                let f = row |> Array.tryFindIndex (fun x -> x = chr)
                match f with 
                | None -> loop a (c + 1) 
                | Some x -> x,c
        loop ary 0

    let getSurroundings (map:char[,]) locs =
        let x = locs
                |> List.fold(fun acc loc -> 
                    let n = { x=loc.x; y=(loc.y)-1; pos=map.[loc.y-1, loc.x]; dist= loc.dist+1; }
                    let e = { x=loc.x+1; y=loc.y; pos=map.[loc.y, loc.x+1]; dist= loc.dist+1; }
                    let s = { x=loc.x; y=loc.y+1; pos=map.[loc.y+1, loc.x]; dist= loc.dist+1; }
                    let w = { x=loc.x-1; y=loc.y; pos=map.[loc.y, loc.x-1]; dist= loc.dist+1; }
                    [n; e; s ;w] @ acc
                ) []
                |> List.where(fun x -> x.pos=' ' )
       // printf "acc: %A\n" x
        //System.Console.ReadKey() x
        x

    let flood maze =
        let xx = coordsOf maze 'O'
       

        let rec step locs = 
            printf "%A\n" locs
            let ss = getSurroundings maze locs
            ss |> List.iter(fun x -> Array2D.set maze x.y x.x 'o')
            printmap maze
            printn (ss |> List.maxBy(fun x -> x.dist)).dist
           // System.Console.ReadKey()
            if (List.length ss) = 0 then
                printn (ss |> List.maxBy(fun x -> x.dist)).dist
            else
                step ss
            ()

        
        step  [ { x=(fst xx); y=(snd xx); pos='x'; dist = 0} ]

    let comp = IntCode2("maze")
    let inq = comp.Initialise prog
    let mutable finished = false

    let printStatus msg = 
        () // printAt 1 1 msg

    let printAt x y msg =
        ()

    let mutable startpos = 50, 23
    let mutable endpos = 50, 23
    let mutable location = 50, 23
    let mutable dir = Direction.North
    let wall = "#"
    let corridor = " "
    let mutable steps = 0
    inq.Post (int64 dir)

    let maze = Array2D.init 70 45 (fun _ _ -> ' ')

   

    comp.OutputReady.Add(fun output ->
          if output = -99999L then
              finished <- true
          else 
              printAt (fst location) (snd location) "D"
              dir <-
                  match output, location with
                  | 0L, (x,y) when dir =  Direction.North -> printStatus "Can't go N - trying E"
                                                             printAt (x-29) (y-1) wall
                                                             Array2D.set maze x (y-1) '#' 
                                                             Direction.West
                  | 0L, (x,y) when dir =  Direction.East -> printStatus "Can't go E - trying S"
                                                            printAt (x+30) y wall
                                                            Array2D.set maze (x+1) y '#' 
                                                            Direction.North
                  | 0L, (x,y) when dir =  Direction.South -> printStatus "Can't go S - trying W"
                                                             printAt (x-29) (y+1) wall
                                                             Array2D.set maze x (y+1) '#' 
                                                             Direction.East
                  | 0L, (x,y) when dir =  Direction.West -> printStatus "Can't go W - trying N"
                                                            printAt (x-30) y wall
                                                            Array2D.set maze (x-1) y '#' 
                                                            Direction.South
                  | 1L, (x,y) when dir =  Direction.North -> printStatus ("Going N                 " +  steps.ToString())
                                                             printAt x y corridor
                                                             location <- (x, y-1)
                                                             steps <- steps + 1
                                                             Direction.East
                  | 1L, (x,y) when dir =  Direction.East -> printStatus ("Going E                  " +  steps.ToString())
                                                            printAt x y corridor
                                                            location <- (x+1, y)
                                                            steps <- steps + 1
                                                            Direction.South
                  | 1L, (x,y) when dir =  Direction.South -> printStatus ("Going S                 " +  steps.ToString())
                                                             printAt x y corridor
                                                             location <- (x, y+1)
                                                             steps <- steps + 1
                                                             Direction.West
                  | 1L, (x,y) when dir =  Direction.West -> printStatus ("Going W                  " +  steps.ToString())
                                                            printAt x y corridor
                                                            location <- (x-1, y)
                                                            steps <- steps + 1
                                                            Direction.North

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
                  | 2L, (_,_) -> printAt 10 80  "************************DONE********************"
                                 endpos <- location
                                 // Direction.End
                                 printAt 5 3 ("STEPS : " +  steps.ToString())
                                 finished <- true
                                 Direction.End
                  | _ -> failwith "invalid user"

            //   printAt 0 3 ("Steps:  " +  steps.ToString())
            //   printAt (fst endpos) (snd endpos) "F"
            //   printAt (fst startpos) (snd startpos) "S"

            //   printAt (fst location) (snd location) "D"

              Array2D.set maze (fst endpos) (snd endpos) 'O'
             // Array2D.set maze  (fst startpos) (snd startpos) 'S'
             // Array2D.set maze (fst location) (snd location) "D"


              // System.Console.ReadKey() 
              //if dir <> Direction.End then
              inq.Post (int64 dir)

              if steps = 2000 then  
                flood maze
                finished <- true //printmap maze

         )
        
    while not finished do
        async {
            do! Async.Sleep(100) } |> ignore


// 953

    0