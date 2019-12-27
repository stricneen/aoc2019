module Day14

open Utils

type Chemical = { Name: string; Amount: int }
type Reaction = { In: Chemical list; Out: Chemical }

let day14 = 
    print "Advent of code - Day 14 - Space Stoichiometry"

    let reactionsStr = readLines "./data/day14.txt" |> Array.toList
    let fuel = "FUEL"
    let ore = "ORE"

    let parseChemical (input:string) =
        { Name = input.Substring(input.IndexOf(" ")).Trim(); 
          Amount = int(input.Substring(0, input.IndexOf(" ")).Trim()) }

    let parseReaction (reaction:string) =
        let inputs = reaction.Substring(0, reaction.IndexOf("=>")).Split(",")
                     |> Array.toList
                     |> List.map (fun x -> parseChemical (x.Trim())) 
        let output = reaction.Substring(reaction.IndexOf("=>") + 3).Trim()
        {
            In = inputs; Out = parseChemical output
        }

    let reactions = reactionsStr
                    |> List.map parseReaction

    let roots = reactions
                |> List.where (fun x -> x.In |> List.exists (fun x -> x.Name = ore))
                |> List.map (fun x -> x.Out.Name)

    // 10 ORE => 10 A
    // 1 ORE => 1 B
    // 7 A, 1 B => 1 C
    // 7 A, 1 C => 1 D
    // 7 A, 1 D => 1 E
    // 7 A, 1 E => 1 FUEL

    // 9 ORE => 2 A
    // 8 ORE => 3 B
    // 7 ORE => 5 C
    // 3 A, 4 B => 1 AB
    // 5 B, 7 C => 1 BC
    // 4 C, 1 A => 1 CA
    // 2 AB, 3 BC, 4 CA => 1 FUEL

    let simplify chems = // removes duplication in chemical list
        chems
        |> List.fold (fun a e -> let current = a |> List.tryFind(fun x -> x.Name = e.Name)
                                 match current with
                                 | None -> e :: a
                                 | Some chem -> 
                                 { chem with Amount = chem.Amount + e.Amount } ::
                                  (a |> List.where(fun x -> x.Name <> chem.Name))) []

    let scale chems factor = 
        chems
        |> List.map (fun x -> { x with Amount = x.Amount * factor })
     
    // let resolve chem = 
    //     let creator = reactions |> List.find (fun x -> x.Out.Name = chem.Name)
    //     let factor = int(ceil((chem.Amount |> double) / (creator.Out.Amount |> double)))
    //     scale creator.In factor

    let resolve creator chem =
        let factor = int(ceil((chem.Amount |> double) / (creator.Out.Amount |> double)))
        scale creator.In factor

    // 2 AB, 3 BC, 4 CA => 1 FUEL
    // 4 C, 1 A => 1 CA

    let expandReaction fuel reaction = 
        
        let toBeReplaced = fuel.In |> List.find(fun x -> x.Name = reaction.Out.Name)
        let updated = resolve reaction toBeReplaced
        printf "%A\n" toBeReplaced
        printf "%A\n" (updated)
        //System.Console.ReadKey()
        let chems = updated @ fuel.In |> List.where(fun x -> x.Name <> toBeReplaced.Name)
        {
            fuel 
            with In = simplify chems   
        }

    // check if any items in l1 are in l2
    let listContains l1 l2 =
        let rec c l1' l2' =
            match l1' with
            | [] -> false
            | h::t -> if l2' |> List.contains h then true else c t l2'
        c l1 l2

    let names chems = 
        chems |> List.map (fun x -> x.Name)

    let topological lst = 
        let rec topSort sort sorted = 
            match sort with
            | [] -> sorted
            | _ ->
                //printf "%A\n" sorted
                //System.Console.ReadKey()
                // get all outputs 
                let outputs = sort |> List.fold(fun a e -> [e.Out.Name] @ a ) []
                let next = sort |> List.find(fun x -> not(listContains (names x.In) outputs))
                let remaining = sort |> List.where(fun x -> x.Out.Name <> next.Out.Name)
                topSort remaining ([next] @ sorted)

        topSort lst []
        

    let rec loop fuel (lst: list<Reaction>) =
       match lst with 
       | [] -> fuel
       | h::t -> let r = expandReaction fuel h
                 let simp = { r with In = simplify r.In }
                 loop simp t
         
    let sorted = topological reactions

    printf "Done : %A\n" sorted


    let fuelR = reactions |> List.find(fun x -> x.Out.Name = fuel)
    let rest = sorted |> List.where(fun x -> x.Out.Name <> fuel)
    
    let r = loop fuelR rest // List of reactions


    printf "Done : %A\n" r





    //let o = loop fuelR




    //printf "Done : %A\n" o
    // let r = reactions
    //         |> List.where (fun x -> x.In |> List.exists (fun x -> x.Name = ore))

    //let t = o.In |> List.map resolve
    
    //let s = t |> List.map List.head
   // printf "s : %A\n" s

    //printf "In ORE : %A\n" (s |> List.sumBy (fun x -> x.Amount))



    0