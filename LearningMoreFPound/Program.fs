// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.


open System
open System.IO
open System.Collections
open System.Text
open System.Threading
open System.Text.RegularExpressions
open System.Diagnostics
open Microsoft.FSharp.Reflection

[<EntryPoint>]
let main argv = 

    let wumpusPercent = 0.15
    let pitTrapPercent = 0.05
    let goldPercent = 0.15
    let weaponPercent = 0.15

    printf "Enter dungeon size: "
    let dungeonSize = int32 (Console.ReadLine())
    
    let generator = new Random (DateTime.Now.Millisecond)
    let playerPosition = [|generator.Next(1, dungeonSize); generator.Next(1, dungeonSize);|]

    let wallDungeon size (dungeon:char[,]) =
            let adjustedSize = size
            for i = 0 to adjustedSize do
                dungeon.SetValue( '#', [|0; i;|])
                dungeon.SetValue( '#', [|adjustedSize; i;|])
                dungeon.SetValue( '#', [|i; 0;|])
                dungeon.SetValue( '#', [|i; adjustedSize;|])
            dungeon

    let buildFoggedMap size =
        let allFog = Array2D.create (size + 2) (size + 2) '?'
        wallDungeon (size + 1) allFog

    let buildDungeon size (playerPosition:int[]) = 
        let generator = new Random (DateTime.Now.Millisecond)
        let emptyDungeon = Array2D.create (size + 2) (size + 2) '.'

        let walledDungeon = wallDungeon (size + 1) emptyDungeon

        let entrancePosition = playerPosition
        walledDungeon.SetValue('^', entrancePosition)

        let addElements symbol percent dungeonSize (dungeon:char[,]) =
            let elementNumbers = int32 (Math.Floor(percent * float (size * size)))
            let rec generatePositions numToGenerate dungeonSize (positions:List<(int * int)>) (dungeon:char[,]) =
                if numToGenerate > 0 then
                    let dimX, dimY = generator.Next(1, dungeonSize), generator.Next(1, dungeonSize)
                    if not (List.exists (fun x -> (dimX, dimY) = x) positions) && (dungeon.[dimX, dimY] = '.') then
                        let newPosition = (dimX, dimY)
                        if newPosition = (entrancePosition.[0], entrancePosition.[1]) then
                            printfn "walla"
                        let newList = List.Cons(newPosition, positions)
                        generatePositions (numToGenerate - 1) dungeonSize newList dungeon
                    else
                        generatePositions numToGenerate dungeonSize positions dungeon
                else
                    positions

            let elementPositions = generatePositions elementNumbers dungeonSize List.Empty dungeon
            for item in elementPositions do
                dungeon.SetValue(symbol, [|fst item; snd item|])

            
            printfn "wakka"

        let wumpusNumbers = Math.Floor(wumpusPercent * float (size * size))
        addElements '!' wumpusPercent (size + 1) walledDungeon
        addElements 'W' weaponPercent (size + 1) walledDungeon
        addElements 'P' pitTrapPercent (size + 1) walledDungeon
        addElements '$' goldPercent (size + 1) walledDungeon

        walledDungeon
    
    let dungeon = buildDungeon dungeonSize playerPosition
    let map = buildFoggedMap dungeonSize

    let printPlayerView (playerPosition:int[]) (map:char[,]) =
        let temp = map.[(playerPosition.[0] - 1)..(playerPosition.[0] + 1), (playerPosition.[1] - 1)..(playerPosition.[1] + 1)]
        temp.SetValue('@', [|1; 1|])
        Console.Clear()
        printfn "%A" temp

    let updateMap (playerPosition:int[]) (map:char[,]) (dungeon:char[,]) =
        map.SetValue((dungeon.[playerPosition.[0], playerPosition.[1]]), playerPosition)




    let movePlayer direction (oldPosition:int[]) (dungeon:char[,])=
        let newPosition =
            match direction with
                |'d' -> [|oldPosition.[0]; oldPosition.[1] + 1;|]
                |'a' -> [|oldPosition.[0]; oldPosition.[1] - 1;|]
                |'s' -> [|oldPosition.[0]+ 1; oldPosition.[1];|]
                |'w' -> [|oldPosition.[0]- 1; oldPosition.[1];|]
                | _ -> oldPosition
        if (dungeon.[newPosition.[0], newPosition.[1]] = '#') then
            oldPosition
        else
            newPosition

    let updateGameStatus (playerPos:int[]) (dungeon:char[,]) =
        match (dungeon.[playerPos.[0], playerPos.[1]]) with
            |'W' -> printfn "Handle find a weapon"
            |'$' -> printfn "Handle find gold"
            |'P' -> printfn "Handle find pit"
            |'!' -> printfn "Handle find wumpus"
            |'^' -> printfn "Handle at entrance"
            | _ -> printfn "Handle empty room"

    let rec gameLoop (playerPos:int[]) (map:char[,]) (dungeon:char[,]) =
        
        let updatePlayer (playerPos:int[]) (map:char[,]) (dungeon:char[,]) =
            let input = Console.ReadKey(true).KeyChar
            match input with
                |'d' | 'a' | 's' | 'w' ->   let newPos = movePlayer input playerPos dungeon
                                            updateMap newPos map dungeon
                                            updateGameStatus newPos dungeon
                                            gameLoop newPos map dungeon
                |'l' -> printfn "Handle looting gold"
                        gameLoop playerPos map dungeon
                |'r' -> printfn "Handle exiting dungeon"
                        gameLoop playerPos map dungeon
                |'x' -> printfn "Handle quick exit game"
                |'?' -> printfn "Handle printing commands"
                        gameLoop playerPos map dungeon
                | _ ->  printfn "do nothing"
                        gameLoop playerPos map dungeon
                            
        
        printPlayerView playerPos map
        updatePlayer playerPos map dungeon
        


    gameLoop playerPosition map dungeon
    printf "\nPress any key to continue..."
    Console.ReadKey(true) |> ignore
    0 // return an integer exit code
