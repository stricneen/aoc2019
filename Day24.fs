module Day24

open Utils
open IntCode2

let day24 = 
    print "Advent of code - Day 24 - Planet of Discord"

    let read2DArray path = 
        let input = readLines path
        let a2d = array2D input
        a2d
        
    let prog = read2DArray "./data/day24.txt" 

    let hasAbove (grid: char[,]) x y =
        x > 0 && grid.[x - 1, y] = '#'

    let hasBelow grid x y = 
        x < Array2D.length1 grid - 1 && grid.[x + 1, y] = '#'

    let hasLeft  (grid: char[,]) x y =
        y > 0 && grid.[x, y - 1] = '#'

    let hasRight grid x y = 
        y < Array2D.length2 grid - 1 && grid.[x, y + 1] = '#'

    let isBug (grid: char[,]) x y =
        grid.[x,y] = '#'

    let adjCount grid x y =
        [
            hasAbove grid x y;
            hasBelow grid x y;
            hasLeft grid x y;
            hasRight grid x y
        ]
        |> List.sumBy (fun x -> if x then 1 else 0)

    let toBeBug grid x y =
        let adj = adjCount grid x y
        match adj with
        | 1 when isBug grid x y -> '#'
        | 1 when not (isBug grid x y) -> '#'
        | 2 when not (isBug grid x y) -> '#'
        | _ -> '.'
        
        
    let ite grid = 
        grid
        |> Array2D.mapi (fun x y e -> toBeBug grid x y )
        
    // biodiversity
    let bioArray grid =
        grid
        |> Array2D.mapi (fun x y e ->
            match e with
            | '.' -> 0
            | _ -> pown 2 (x * 5 + y) 
        )
        |> Seq.cast<int>
        |> Seq.sum
        
    


    let rec iterate grid c iterations = 
        //  printf "%A\n\n" grid
        //  printf "%A\n\n" (bioArray grid)
        //  System.Console.ReadKey() |> ignore 
        let newGrid = ite grid
        printmap newGrid
        let bio = bioArray newGrid
        if List.contains bio iterations  then
            printf "Repeated %A\n" bio
        else
            iterate newGrid (c + 1) (bio :: iterations)

    let bio = bioArray prog
    // let x = iterate prog 0 [bio]


    // PART 2

    let emptyGrid = Array2D.init 5 5 (fun _ _ -> '.')

    let extend grids = 
       let s = List.append [emptyGrid] grids
       List.append s [emptyGrid]

    let above (grid:char[,]) x y = 
        let u = if y = 0 && grid.[2,1] = '#' then 1 else 0
        let d = if y = 4 && grid.[2,3] = '#' then 1 else 0
        let l = if x = 0 && grid.[1,2] = '#' then 1 else 0
        let r = if x = 4 && grid.[3,2] = '#' then 1 else 0
        u + d + l + r


    let below (grid:char[,]) x y =

        let sum line =
            let nums = line
                        |> Array.map(fun x -> if x = '#' then 1 else 0)
            //printf "%A\n" nums
            nums |> Array.sum

        let u = if x = 2 && y = 1 then sum grid.[*,0] else 0
        let d = if x = 2 && y = 3 then sum grid.[*,4] else 0
        let l = if x = 1 && y = 2 then sum grid.[0,*] else 0
        let r = if x = 3 && y = 2 then sum grid.[4,*] else 0
        u + d + l + r

    let neighbourCount grids x y = 
        let aboveGrid = grids |> List.head
        let level = grids |> List.skip 1 |> List.head
        let belowGrid = grids |> List.skip 2 |> List.head

        let local = adjCount level x y // bugs on this level
        
        let a = above aboveGrid x y
        let b = below belowGrid x y

        local + a + b

    let tick grids =  
        let level = grids |> List.skip 1 |> List.head
        level
        |> Array2D.mapi(fun x y e -> 
            let neighbours =  neighbourCount grids x y

            if x = 2 && y = 2 then 
                '.'
            else
                match neighbours with
                | 1 when isBug level x y -> '#'
                | 1 when not (isBug level x y) -> '#'
                | 2 when not (isBug level x y) -> '#'
                | _ -> '.'
            //printf "%A %A %A %A\n" x y e neighbour
        )

        
    let rec iterate2 state c = 
        let extended = extend state
                       |> List.windowed 3
                       |> List.map tick
        // printf "%A\n" extended
        // printf "%A\n" ""
        // printf "%A\n" c
        // printf "%A\n" ""
        
       // System.Console.ReadKey()
        match c with
        | 200 -> extended
        | _ -> iterate2 (extend extended) (c+1)

    let bugsInLevel grid = 
        grid
        |> Array2D.map(fun x -> if x = '#' then 1 else 0)
        |> Seq.cast<int>
        |> Seq.sum
        

    let init = extend [prog]
   // printf "%A\n" init

    let output = iterate2 init 1
  //  printf "%A\n" output

    let total = output
                |> List.sumBy bugsInLevel
    
    printf "%A\n" total
    0


// ....#
// #..#.
// #..##
// ..#..
// #....