module Day1

open Utils

let day1 =

    print "Advent of code - Day 1 - The Tyranny of the Rocket Equation"

    let input = readLineInt "./data/day1.txt"

    let requiredFuel mass = ( mass / 3 ) - 2

    // Part 1

    let moduleFuel = input
                     |> Seq.sumBy requiredFuel

    // Part 2

    let rec fuelreq mass = seq {
        let fuel = requiredFuel mass
        match fuel with
        | x when x < 1 -> ()
        | _ -> yield fuel
               yield! fuelreq fuel
    }

    let totalFuel = input
                    |> Seq.sumBy (fun x -> (fuelreq x |> Seq.sum) )

    // Output

    printn moduleFuel
    printn totalFuel
