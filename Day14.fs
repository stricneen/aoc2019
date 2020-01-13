module Day14

open Utils

type Chemical = { Name: string; Amount: int }
type Reaction = { In: Chemical list; Out: Chemical }

let day14 = 
    print "Advent of code - Day 14 - Space Stoichiometry"

    let reactionsStr = readLines "./data/day14a.txt" |> Array.toList
    let fuel = "FUEL"
    //let ore = "ORE"

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

    let resolve creator chem =
        let factor = int(ceil((chem.Amount |> double) / (creator.Out.Amount |> double)))
        scale creator.In factor

    let expandReaction fuel reaction = 
        printf "t : %A\n\n" reaction
        let toBeReplaced = fuel.In |> List.find(fun x -> x.Name = reaction.Out.Name)
        let updated = resolve reaction toBeReplaced
        //printf "t : %A\n\n" toBeReplaced
       // printf "u : %A\n\n\n" (updated)
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


    let fuelR = reactions |> List.find(fun x -> x.Out.Name = fuel)
    let rest = sorted |> List.where(fun x -> x.Out.Name <> fuel)
    
    let r = loop fuelR rest // List of reactions


    printf "Done : %A\n" r


    let trillion = 1000000000000L

    printf "%A\n" (trillion / 13312L)

   // let top = 

   // The 13312 ORE-per-FUEL example could produce 82892753 FUEL.

    // let s = 13312L
    // let mutable x = 0L
    // let mutable fuel = 0L

    // while x < trillion do
    //     x <- x + s
    //     fuel <- fuel + 1L

    // printf "f : %A\n" fuel

       





    0


    82892753
    75120192