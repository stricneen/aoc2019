module Day21

open Utils
open IntCode2





let day21 =
    print "Advent of code - Day 21 - Springdroid Adventure"

    let prog = readCSV "./data/day21.txt" 

    let comp = IntCode2 "bot"
    let inq = comp.Initialise prog
    
    let mutable finished = false
    let mutable buffer = ""

 //   inq.Post 30L
    
    comp.OutputReady.Add(fun output -> 
        if output = 10L then
            print buffer
            //print "\n"
            buffer <- ""
        else if output > 255L then
            printf "%A\n" output
        else
            buffer <- buffer + (char(output |> int)).ToString()
        )
          

    let p1 = [
        "NOT A T";
        "AND D T";
        "OR T J";
        "NOT B T"; 
        "AND D T"; 
        "OR T J"; 
        "NOT C T"; 
        "AND D T";
        "AND H T"; 
        "OR T J"; 
        "RUN"
    ]
            
    let part1 = [
        "NOT C T";
        "AND D T";
        "NOT A J";
        "OR T J";
        "WALK";
    ]

    let part2 = [

        "NOT C J";
        "AND D J";

        "OR H T";
        "OR I T";
        "AND T J";

        "NOT A T"; //JUMP IF HOLE NEXT
        "OR T J";

        "NOT F T";
        //"AND C T";
        "AND D T";
        //"AND G T";
        "AND H T";
        //"AND I T";
        "OR T J";

        "NOT B T";
        "AND C T";
        "AND D T";

        "OR T J";
       

        "RUN"
    ]

(*
........@........
#####..###.##.###
         ABCDEFGHI

@................
#####.#.#.##..###
 ABCDEFGHI
....@............
#####.##...#..###


               J
                ABCDEFGHI
            #####..X.########
             #####.X..########
             #####.X#.########
             #####.X.#.##..###
           #####.#.X...#.###
             #####.X#...#..###
         #####.J#.##..####
                ABCDEFGHI

........@........
#####..###.##.###
         ABCDEFGHI


(-C D HvI) v (c d g h i)

             ABCDEFGHI
            #####.###########
            #####...#########
            #####..#.########
            #####.#..########
            #####.##.########
            #####.#.#.##..###
            #####.#.#...#.###
            #####.##...#..###      
            #####.##.##..####
            #####.#.#..######
            #####..###.##.###
*)
    
     p1
        |> List.iter(fun command ->
            for x in command do
                let i = (int64 x)
                inq.Post i
            inq.Post 10L
        
        )

    // Manual entry
    //while not finished do
        // manual
        // let command = System.Console.ReadLine()
        // for x in command do
        //     let i = (int64 x)
        //     inq.Post i
        // inq.Post 10L
        
        
    System.Console.ReadKey() |> ignore
    async {
        do! Async.Sleep(100) } |> ignore


    0

