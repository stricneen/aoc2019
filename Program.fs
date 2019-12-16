[<EntryPoint>]
let main argv =

    //Day1.day1 |> ignore

    //Day2.day2 |> igno

    //Day3.day3 |> ignore

    //Day4.day4 |> ignore

    //Day5.day5 |> ignore

    //Day6.day6 |> ignore

    //Day7.day7 |> ignore

    //Day8.day8 |> ignore
    
    //Day9.day9 |> ignore

    //Day10.day10 |> ignore

    //Day11.day11 |> ignore

    //Day12.day12 |> ignore



    let waitforit input = 
        let signal = input

        while true do
            async {
                printf "Waiting for input ... \n"
                let i = signal // Should wait here unitl input func is call outside
                printf "Input rec: %A\n" i
            } |> ignore

    let pass v = v

    let wfi = waitforit (pass)

    System.Console.ReadKey() |> ignore
    pass 345 |> ignore

    System.Console.ReadKey() |> ignore
    pass 475 |> ignore




    0 // return an integer exit code
