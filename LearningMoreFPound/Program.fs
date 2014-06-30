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
                        let newList = List.Cons(newPosition, positions)
                        generatePositions (numToGenerate - 1) dungeonSize newList dungeon
                    else
                        generatePositions numToGenerate dungeonSize positions dungeon
                else
                    positions

            let elementPositions = generatePositions elementNumbers dungeonSize List.Empty dungeon
            for item in elementPositions do
                dungeon.SetValue(symbol, [|fst item; snd item|])

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
        printfn "%A" temp

    let updateMap (playerPosition:int[]) (map:char[,]) (dungeon:char[,]) =
        map.SetValue((dungeon.[playerPosition.[0], playerPosition.[1]]), playerPosition)

    let obliterateWeapons (map:char[,]) (dungeon:char[,])=
        for i = 0 to dungeonSize do
            for j = 0 to dungeonSize do
                if dungeon.[i,j] = 'W' then
                    dungeon.SetValue('$', [|i; j;|])
                    if map.[i,j] <> '?' then
                        updateMap [|i; j;|] map dungeon


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

    let playerLoot (playerPos:int[]) (map:char[,]) (dungeon:char[,]) =
        dungeon.SetValue('.', playerPos.[0], playerPos.[1])
        updateMap playerPos map dungeon

    let rec gameLoop (playerPos:int[]) playerScore playerArmed (map:char[,]) (dungeon:char[,]) =
        
        printfn "%A \n" map
        printfn "%A" dungeon
        let input = Console.ReadKey(true).KeyChar
        Console.Clear()
        //printPlayerView playerPos map
        match input with
            |'d' | 'a' | 's' | 'w' ->   let newPos = movePlayer input playerPos dungeon
                                        updateMap newPos map dungeon
                                        match (dungeon.[newPos.[0], newPos.[1]]) with
                                        | '!' ->    printfn "Handle meeting a Wumpus (+10 points or death)"
                                        | 'P' ->    printfn "Handle meeting a Pit (death)"
                                        | '.' ->    printfn "Handle entering empty room (+1 point or nothing)"
                                        | _ ->      printfn "do nothing"
                                        gameLoop newPos playerScore playerArmed map dungeon
            |'l' -> match (dungeon.[playerPos.[0], playerPos.[1]]) with
                        |'W' -> playerLoot playerPos map dungeon
                                obliterateWeapons map dungeon
                                gameLoop playerPos (playerScore + 5) true map dungeon
                        |'$' -> playerLoot playerPos map dungeon
                                gameLoop playerPos (playerScore + 5) playerArmed map dungeon
                        |_ ->   printfn "You cannot loot that"
                                gameLoop playerPos playerScore playerArmed map dungeon
            |'r' -> printfn "Handle exiting dungeon"
                    printfn "You escaped the Wumpus cave with %d points" playerScore
            |'x' -> printfn "Handle quick exit game"
            |'?' -> printfn "Handle printing commands"
                    gameLoop playerPos playerScore playerArmed map dungeon
            | _ ->  printfn "do nothing"
                    gameLoop playerPos playerScore playerArmed map dungeon
                            
        
        
        

    updateMap playerPosition map dungeon
    gameLoop playerPosition 0 false map dungeon
    printf "\nPress any key to continue..."
    Console.ReadKey(true) |> ignore
    0 // return an integer exit code
