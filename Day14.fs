module Day14

open Utils

type Chemical = { Name: string; Amount: int }
type Reaction = { In: Chemical array; Out: Chemical }

let day14 = 
    print "Advent of code - Day 14 - Space Stoichiometry"

    let reactions = readLines "./data/day14a.txt" 

    let parseChemical (input:string) =
        { Name = input.Substring(input.IndexOf(" ")).Trim(); 
          Amount = int(input.Substring(0, input.IndexOf(" ")).Trim()) }

    let parseReaction (reaction:string) =
        let inputs = reaction.Substring(0, reaction.IndexOf("=>")).Split(",")
                     |> Array.map (fun x -> parseChemical (x.Trim())) 
                   
                     
        let output = reaction.Substring(reaction.IndexOf("=>") + 3).Trim()
        {
            In = inputs; Out = parseChemical output
        }
        

    //let r =  "7 A, 1 D => 1 E"
    // printf "r : %A\n" (r.Substring(r.IndexOf("=>") + 3).Trim())
    // printf "amount : %A\n" ("7 A".Substring("7 A".IndexOf(" ")).Trim())

    //printf "r : %A\n" (parseChemical "7 A")


    reactions
    |> Array.map parseReaction
    |> Array.iter (fun x -> printf "%A\n" x)

  
  

// 10 ORE => 10 A
// 1 ORE => 1 B
// 7 A, 1 B => 1 C
// 7 A, 1 C => 1 D
// 7 A, 1 D => 1 E
// 7 A, 1 E => 1 FUEL







    0