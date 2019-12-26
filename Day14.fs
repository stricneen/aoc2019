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


    let scale chems factor = 
        chems
        |> List.map (fun x -> { x with Amount = x.Amount * factor })
     
    let resolve chem = 
        let creator = reactions |> List.find (fun x -> x.Out.Name = chem.Name)
        let factor = int(ceil((chem.Amount |> double) / (creator.Out.Amount |> double)))
        scale creator.In factor

    let withOutput chem = 
        if roots |> List.exists (fun x -> x = chem.Name) then
            List.singleton chem
        else
           resolve chem

    let simplify chems =
        chems
        |> List.fold (fun a e -> let current = a |> List.tryFind(fun x -> x.Name = e.Name)
                                 match current with
                                 | None -> e :: a
                                 | Some chem -> 
                                 { chem with Amount = chem.Amount + e.Amount } ::
                                  (a |> List.where(fun x -> x.Name <> chem.Name))) []

    let expandReaction reaction = 
        let chems = reaction.In
                    |> (List.fold (fun a e -> List.append a (withOutput e)) [])
        printf "%A\n" chems
        printf "%A\n" (simplify chems)
        {
            reaction 
            with In = simplify chems   
        }

    let fuelR = reactions |> List.find(fun x -> x.Out.Name = fuel)

    let rec loop reaction = 
        if reaction.In |> List.forall(fun x -> roots |> List.contains x.Name) then 
            reaction
        else
            let exp = expandReaction reaction
            loop exp

    let o = loop fuelR

    printf "Done : %A\n" o
    // let r = reactions
    //         |> List.where (fun x -> x.In |> List.exists (fun x -> x.Name = ore))

    let t = o.In |> List.map resolve
    
    printf "res : %A\n" t




    let s = t |> List.map List.head
    printf "s : %A\n" s

    printf "In ORE : %A\n" (s |> List.sumBy (fun x -> x.Amount))



    0