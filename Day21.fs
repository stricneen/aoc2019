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
          
            
    let part1 = [
        "NOT C T";
        "AND D T";
        "NOT A J";
        "OR T J";
        "WALK";
    ]

    let part2 = [

// A B F -G

// E v F v G
        // "OR A T";
        // "AND B T";
        // "AND C T";
        // "AND D T";
        // "NOT T J";

        "NOT E J";
        "AND D J";
        "AND H J";
      //  "AND I J";
       // "";
       // "";
       //  "AND T J";

        "NOT C T";
        "AND D T";
        "OR T J";

        "NOT A T"; //JUMP IF HOLE NEXT
        "OR T J";

        "AND D J"; // DONT JUMP IF HOLE +4
        // "NOT E T";
        // "AND H T";

        // //EGI
        // "OR E J";
        // "AND G J";
        // "AND I J";
        // "OR J T";
       
        // "NOT A J";
        // "OR T J";

        // "AND D J";

      
        

        "RUN"
    ]
(*
..@..............
#####.#.#...#.###
   ABCDEFGHI


..@..............
#####.#.#...#.###
   ABCDEFGHI

..@..............
#####.#.#...#.###

              #####.###########
            #####...#########
             #####..#.########
              #####.#..########
             #####.##.########
              #####.#.#.##..###
            #####.#.#...#.###


    

@................
#####.#.#.##..###
 ABCDEFGHI

            #####.###########
            #####...#########
            #####..#.########
            #####.#..########
            #####.##.########
            #####.#.#.##..###
            #####.#.#...#.###
            
              ABCDEFGHI
             #####.#.#.##..###  

                ABCDEFGHI
             #####.#.#.##..###     
                #####..#.########
      #####..#.########


                ABCDEFGHI
             #####.X.#.##..###
           #####.#.X...#.###
        
A B F -G

D -E H  AND I 

            #####..X.########
             #####.X..########
            #####.#X.########
             #####.X.#.##..###
           ..@..............
              ABCDEFGHI
           #####.#.X...#.###


                    I  I

#####..#.########
#####.#..########
#####.##.########
 ABCDEFGHI
#####.#.#.##..###

*)
    
     part2
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

