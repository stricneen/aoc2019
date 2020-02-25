module Day18

open Utils
open System

type Location = { x:int; y:int; pos:char; dist:int; doors: char list }

type State = { total:int; map: char[,] }

type Key = { key: char; dist: int; doors: char list }

type Path = { id: int; at: string; visited: string; travelled: int; remaining: Key list }

let day18 = 
    print "Advent of code - Day 18 - Many-Worlds Interpretation"

    let inline pt x = printf "%A\n" x

    let inline ptc x = printf "%A\n" x
                       Console.ReadKey() |> ignore

    let read2DArray path = 
        let input = readLines path |> Array.takeWhile (fun x -> x <> "*")
        let a2d = array2D input 
        a2d
        
    let coordsOf ary chr =
        let rec loop (a: char[]) acc r c =
            let f = a |> Array.tryFindIndex (fun x -> x = chr)
            let x = match f with 
                    | None -> acc 
                    | Some x -> let ext = a.[(x+1)..]
                                let next = loop ext [x+c,r] r (x+c+1) @ acc
                                acc @ next
            x

        let rec loopRows ary r acc =
            if r > Array2D.length1 ary - 1 then
                acc
            else
                let row = ary.[r, *]
                let found = loop row [] r 0
                loopRows ary (r+1) (acc @ found)

        let coords = loopRows ary 0 []
        // [chr, (coords)]
        coords |> List.distinct |> List.map(fun x -> chr, x)
 
    let rec printmap lst = 
        for x in 0 .. Array2D.length1 lst - 1 do
            let l = lst.[x,*] |> String
            printf "%A\n" l
  
    // #########
    // #b.A.@.a#
    // #########
    
    let doors visitied current = 
        if System.Char.IsUpper current then // || System.Char.IsLower current then //  || System.Char.IsLower current then
            visitied @ [current]
        else
            visitied

    let getSurroundings (map:char[,]) locs =
        let x = locs
                |> List.fold(fun acc loc -> 
                    let n = { x=loc.x; y=(loc.y)-1; pos=map.[loc.y-1, loc.x]; dist= loc.dist+1; doors= doors loc.doors map.[loc.y-1, loc.x] }
                    let e = { x=loc.x+1; y=loc.y; pos=map.[loc.y, loc.x+1]; dist= loc.dist+1; doors= doors loc.doors map.[loc.y, loc.x+1] }
                    let s = { x=loc.x; y=loc.y+1; pos=map.[loc.y+1, loc.x]; dist= loc.dist+1; doors= doors loc.doors map.[loc.y+1, loc.x] }
                    let w = { x=loc.x-1; y=loc.y; pos=map.[loc.y, loc.x-1]; dist= loc.dist+1; doors= doors loc.doors map.[loc.y, loc.x-1] }
                    [n; e; s ;w] @ acc
                ) []
                |> List.where(fun x -> x.pos='.' || x.pos='@' || System.Char.IsLower x.pos || System.Char.IsUpper x.pos)
       // printf "acc: %A\n" x
        //System.Console.ReadKey() x
        x
                
    let availableKeys map from = 
        let rec traverse (points: Location list) c = 
            let ends = points |> List.where(fun x -> x.dist = c)
            let x = getSurroundings map ends @ points
                    |> List.sortBy(fun x -> x.dist)
                    |> List.distinctBy(fun x -> x.x * 1000 + x.y)
            if List.length x = List.length points then
                x
            else 
                traverse x (c+1)

        let key = fst from
        let start = snd from


        let locations = traverse [ { x=fst start; y=snd start; pos=key; dist=0; doors= [] }] 0
        //pt locations
        locations |> List.where(fun x -> System.Char.IsLower x.pos)
        

    let printState s = 
        s |> List.iter(fun x -> printf "Total : %A\n" x.total
                                printmap x.map
        )

    let prog = read2DArray "./data/day18_2.txt"
    
    // Get all keys and coords
    let getKeys map =   
        let x = [ 'a' .. 'z' ] @ ['@']
                |> List.fold (fun a x -> a @ (coordsOf map x )) []
        x 

    let locsToKeys locs = 
        locs 
        |> List.map(fun x -> { key = x.pos; dist = x.dist; doors = x.doors |> List.map Char.ToLower })
        |> List.sortBy (fun x -> List.length x.doors)

    let distances map keys =
        keys 
        |> List.map(fun x -> (x, availableKeys map x )
               >> (fun (key, locs) -> key, locsToKeys locs))      

    let keys = getKeys prog


    //pt keys

    let dists = distances prog keys
                |> List.map(fun (x,y) -> fst x, y)

    let bots = dists |> List.where(fun (x,_)-> x = '@')

    // make the first moves

    let first = bots
                |> List.mapi(fun i (x, keys) -> 
                    { 
                    id = i;
                    at = x.ToString();
                    visited = ""; //x.key.ToString(); 
                    travelled = 0;  //x.dist; 
                    remaining = (keys)
                                //|> List.where(fun x' -> x'.key <> x.key))  // Remove just visited
                                //|> List.map(fun x' -> { x' with doors = x'.doors |> List.where(fun x'' -> x'' <> x.key); dist = keyToKey x.key x'.key  }) }) // Remove door
                    })
    
    // pt first 

    let traverse (start: Path list) dists = 

        // Get the distance between two keys
        let keyToKey k1 k2 =
            let k1' = dists
                      |> List.find(fun (x, _) -> x = k1)   // 'char * Key list' 
            let k2' = (snd k1')
                      |> List.find(fun x -> x.key = k2)
            k2'.dist

        let botMove state bot =

            let others = state |> List.where(fun x -> x <> bot)

            let canMove = bot.remaining
                               |> List.where(fun x -> List.isEmpty x.doors)
                               |> List.sortBy(fun x -> x.key)

            let moves =
                canMove |> List.map(fun x -> { 
                id = bot.id;
                at = x.key.ToString();
                visited = bot.visited + x.key.ToString(); 
                travelled = x.dist + bot.travelled; 
                remaining = (bot.remaining 
                            |> List.where(fun x' -> x'.key <> x.key))  // Remove just visited
                            |> List.map(fun x' -> { x' with doors = x'.doors |> List.where(fun x'' -> x'' <> x.key); dist = keyToKey x.key x'.key  }) }) // Remove door

            let removeKey key remaining = 
                remaining
                |> List.map (fun r ->
                    { r with doors = r.doors |> List.where(fun d -> d.ToString() <> key)}
                )

            let newStates = 
                moves
                |> List.map(fun m -> 
                    let removeKeys = others
                                     |> List.map(fun k -> { k with remaining = removeKey m.at k.remaining })

                    [m] @ removeKeys
                )
            newStates

        
        let rec move from =
            
            let s = from 
                    |> List.fold (fun acc state -> 
                        let newState = state
                                        |> List.fold(fun a s -> 
                                            a @ (botMove state s)
                                        ) []
                        newState @ acc
                        ) []
                    
            let optimize bot =
                //pt bot
                let start = bot |> List.where(fun x -> x.visited.Length = 0)
                let moved = bot |> List.where(fun x -> x.visited.Length > 0)
                
                let m = moved |> List.map(fun x -> 
                        let hash = x.visited.Substring(0, x.visited.Length - 1) |> Seq.sort |> String.Concat
                        //pt hash
                        hash, x
                        )
                        |> List.groupBy(fun x -> fst x, (snd x).at)
                        |> List.map((fun (_,x) -> x |> List.minBy(fun (_,y) -> y.travelled))
                                >> (fun (_,x) -> x))
                m @ start

            let getHash s =
                s
                |> List.sortBy(fun x -> x.id)
                |> List.fold(fun a x ->
                    a + x.at + (x.visited |> Seq.sort |> String.Concat)
                 ) ""

            let getTravelled s =
                s |> List.sumBy(fun x -> x.travelled) 
                 

            let optimize2 state =
                let x = state
                        |> List.map(fun x -> getHash x, getTravelled x , x)
                        |> List.groupBy fst3
                        |> List.map(fun (_,x) -> x |> List.minBy snd3)
                        |> List.map(fun (_,_,x) -> x)
                
                
                
                //ptc x
                x
                

                      
           
// list<string * list<string * int * list<Path>>>

                


            //let s' = s |> List.map optimize

            let shortest = optimize2 s

            // printf " > %A\n" (List.length s)
            // printf " > > > %A\n" (List.length shortest)
            

            //ptc shortest
            //Console.ReadKey()


           // printn (s |> List.concat |> List.length)
            printn (shortest |> List.concat |> List.length)
            // pt shortest
            if (shortest |> List.concat |> List.forall(fun x -> List.isEmpty x.remaining)) then
                shortest
            else 
                move shortest

        move [start]

    let x = traverse first dists// collect all the keys
    //pt x

    let distances = x |> List.map(fun y -> y |> List.sumBy(fun y' -> y'.travelled))

    let shortest = distances |> List.min
    pt distances


    //let min = x |> List.minBy(fun x -> x.travelled)  // get shortest route
    printf "Shortest  : %A\n" shortest

    0




// 9
// 76
// 604
// 4520
// 31732
// 207264
// 1244648

// 36
// 304
// 2416
// 18080
// 126928
// 829056
// 4978592