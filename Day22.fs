module Day22

open Utils

// deal into new stack
// deal with increment 21
// cut -1639

let day22 = 
    print "Advent of code - Day 22 - Slam Shuffle"

    let ins = readLines "./data/day22.txt" |> Array.toList

    let startDeck = [ 0 .. 9 ]

    printf "%A\n" startDeck

    let getNumber (line:string) =
        int(line.Split(' ') |> Array.last)


    let cut (deck:list<int>) (pos:int) = 
        deck.[pos..] @ deck.[0..pos-1]

    let shuffle deck (line: string) =
        match line with
        | l when l.Contains("stack") -> deck |> List.rev
        | l when l.Contains("increment") -> deck |> List.rev
        | l when l.Contains("cut") -> cut deck (getNumber line)
        | _ -> failwith "Unknown shuffle"
        

    // let ns = shuffle startDeck "deal into new stack"
    let ns = shuffle startDeck "cut 3"
  
    printf "%A\n" ns
  


    











    0