module Day21

open Utils
open IntCode2

(*

ANSWER : 
Input instructions:
NOT C T
AND D T
NOT A J
OR T J
WALK




            #####.###########
            #####...#########


              ABCDEFGHI
             #####.#.#.##..###  

                ABCDEFGHI
             #####.#.#.##..###     
                #####..#.########
      
                ABCDEFGHI
           #####.#.#...#.###
            #####..#.########
             #####.#..########
            #####.##.########

.................
.................
@................
#####.#.#.##..###
 ABCDEFGHI



 NOT I J
 and a T
 AND J T
 

 

  not e J
  and d J
  and h J


OR J T

NOT A J
OR T J

 ABCDE
J##.#.
J#.##.

NOT C J
NOT E T
AND D T
AND J T

NOT A J
OR T J

RUN













*)




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
          
            
    let instructions1 = [
        "NOT C T";
        "AND D T";
        "NOT A J";
        "OR T J";
        "WALK";
    ]

    let instructions2 = [
        // "OR I J";
        // "OR E J";
        // "OR J T";

        "NOT I J";
        "AND A T";
        "AND J T";

        "NOT E J";
        "AND D J";
        "AND H J";

        "OR J T";


        "NOT A J";
        "OR T J";

        "RUN"
    ]
    
     instructions2
        |> List.iter(fun command ->
            for x in command do
                let i = (int64 x)
                inq.Post i
            inq.Post 10L
        
        )

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

