module Day14

open Utils

type Chemical = { Name: string; Amount: int64 }
type Reaction = { In: Chemical list; Out: Chemical }

let day14 = 
    print "Advent of code - Day 14 - Space Stoichiometry"

    let reactionsStr = readLines "./data/day14.txt" |> Array.toList
    let fuel = "FUEL"
    //let ore = "ORE"
   
    let fx fr x = 
        { In = fr.In |> List.map(fun fr -> { fr with Amount = fr.Amount * x}); Out = { fr.Out with Amount = fr.Out.Amount * x }}

    let parseChemical (input:string) =
        { Name = input.Substring(input.IndexOf(" ")).Trim(); 
          Amount = int64(input.Substring(0, input.IndexOf(" ")).Trim()) }

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
        let factor = int64(ceil((chem.Amount |> double) / (creator.Out.Amount |> double)))
        scale creator.In factor

    let expandReaction fuel reaction = 
        // printf "t : %A\n\n" reaction
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

    let fuelReaction = reactions |> List.find(fun x -> x.Out.Name = fuel)
    let rest = sorted |> List.where(fun x -> x.Out.Name <> fuel)
    
    let howMuchOre forThisFuel =
        let fuelx = fx fuelReaction forThisFuel
        let r = loop fuelx rest
        r

    // The 13312 ORE-per-FUEL example could produce 82892753 FUEL.
    // The 180697 ORE-per-FUEL example could produce 5586022 FUEL.
    // The 2210736 ORE-per-FUEL example could produce 460664 FUEL.

    let trillion = 1000000000000L

    let getBounds = 
        let rec l f =
            let ore = howMuchOre f
            if (ore.In |> List.head).Amount > trillion then
                (f/2L),f
            else
                l (f*2L)
        l 1L

    let calc = 
        let rec x (l,u) =
            let h = l + ((u-l)/2L)
            let ore = howMuchOre h
            if l = h then 
                (l,u)
            else
                // printf "t : %A :  %A\n" (l,h) (h,u)
                if (ore.In |> List.head).Amount > trillion then
                    x (l,h)
                else
                    x (h,u)
        x getBounds

    let t = calc
    printf "Total : %A\n" (fst t)



    // let l = howMuchOre 1000000L
    // let u = howMuchOre trillion


    // printf "%A\n" l
    // printf "%A\n" u
    // let getBounds =
    //     3,40

    // let onetrillion =
    //     let rec l c =
    //         let r = howMuchOre c
    //         //printf "%A\n" r
    //         if r.Out.Amount > trillion then    
    //             print "Done"
    //         else
    //             l (c+1L)
    //         ()
    //     ()
       // l 0L

        
  
    

// ans : 82892753
// div : 75120192


    0
