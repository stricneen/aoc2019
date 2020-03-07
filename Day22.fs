module Day22

open Utils

// deal into new stack
// deal with increment 21
// cut -1639

let day22 = 
    print "Advent of code - Day 22 - Slam Shuffle"

    let ins = readLines "./data/day22.txt" |> Array.toList

    let startDeck = [ 0 .. 10006 ]

    //printf "%A\n" startDeck

    let getNumber (line:string) =
        int(line.Split(' ') |> Array.last)


    let cut (deck:list<int>) (pos:int) = 
        if pos > 0 then
            deck.[pos..] @ deck.[0..pos-1]
        else
            let len = List.length deck
            (deck |> List.skip (len + pos)) @ (deck |> List.take (len + pos))

    let updateEle (lst:list<int>) index itm =
        lst.[0..index - 1] @ [ itm ] @ lst.[index + 1 ..]

    let increment (deck:list<int>) (pos:int) =
        
        let len = List.length deck
        let mutable nw = List.init len (fun x -> 0)
        for x in 0 .. len - 1 do
            let pos = (x * pos) % len
            //printf "%A\n" pos
            nw <- updateEle nw pos deck.[x] 
        nw              
        
    let shuffle deck (line: string) =
        match line with
        | l when l.Contains("stack") -> deck |> List.rev
        | l when l.Contains("increment") -> increment deck (getNumber line)
        | l when l.Contains("cut") -> cut deck (getNumber line)
        | _ -> failwith "Unknown shuffle"
        

    // let ns = shuffle startDeck "deal into new stack"
    // let ns = shuffle startDeck "cut -4"
    // let ns = shuffle startDeck "deal with increment 3"
  
    let rec shf deck c =
        if c > (List.length ins) - 1 then
            deck
        else
            let instr = ins.[c]
            print instr
            let shuf = shuffle deck instr
            shf shuf (c + 1)


    let r = shf startDeck 0

    // printf "%A\n" r
  
    let pos = r |> List.tryFindIndex(fun x -> x = 2019)
    printf "pos : %A\n" pos


    











    0