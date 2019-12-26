module Day14

open Utils

type Chemical = { Name: string; Amount: int }
type Reaction = { In: Chemical list; Out: Chemical }

let day14 = 
    print "Advent of code - Day 14 - Space Stoichiometry"

    let reactionsStr = readLines "./data/day14a.txt" |> Array.toList
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

// 9 ORE => 2 A
// 8 ORE => 3 B
// 7 ORE => 5 C
// 3 A, 4 B => 1 AB
// 5 B, 7 C => 1 BC
// 4 C, 1 A => 1 CA
// 2 AB, 3 BC, 4 CA => 1 FUEL

// 10 ORE => 10 A
// 1 ORE => 1 B
// 7 A, 1 B => 1 C
// 7 A, 1 C => 1 D
// 7 A, 1 D => 1 E
// 7 A, 1 E => 1 FUEL

    let mutable store = []

    let withOutput chem = 
        match chem.Name with
        | "ORE" -> List.singleton chem
        | _ -> (reactions |> List.find (fun x -> x.Out.Name = chem.Name)).In


        

    let rec getRequired chem =
        let checkStorage = store |> List.tryFind(fun x -> x.Name = chem.Name)
        store <- match checkStorage with
                 | None -> [withOutput chem] :: store
                 | Some inStore -> if inStore.Amount > chem.Amount then 
                                        { Name=chem.Name; Amount=inStore.Amount - chem.Amount } :: store |> List.where(fun x -> x.Name != chem.Name)
                                     else
                                         (withOutput chem) :: store



    let withOutput chem = 
        match chem.Name with
        | "ORE" -> List.singleton chem
        | _ -> (reactions |> List.find (fun x -> x.Out.Name = chem.Name)).In
    
    let expandReaction reaction = 
        {
            In = reaction.In
                 |> (List.fold (fun a e -> List.append a (withOutput e)) [])  ;
            Out = reaction.Out
        }

    let fuelR = reactions |> List.find(fun x -> x.Out.Name = fuel)

    let rec resolve reaction = 
        if reaction.In |> List.forall(fun x -> x.Name = ore) then 
            reaction
        else
            printf "%A\n" reaction
            let exp = expandReaction reaction
            // System.Console.ReadKey() |> ignore
            resolve exp

    let o = resolve fuelR
    printf "Done : %A\n" o

    printf "In ORE : %A\n" (o.In |> List.sumBy (fun x -> x.Amount))




    //let r =  "7 A, 1 D => 1 E"
    // printf "r : %A\n" (r.Substring(r.IndexOf("=>") + 3).Trim())
    // printf "amount : %A\n" ("7 A".Substring("7 A".IndexOf(" ")).Trim())
    //printf "r : %A\n" (parseChemical "7 A")


  
    







    0