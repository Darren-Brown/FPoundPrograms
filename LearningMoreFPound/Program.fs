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

type cellState = {content:char; visible:bool}

[<EntryPoint>]
let main argv = 

    let wumpusPercent = 0.15
    let pitTrapPercent = 0.05
    let goldPercent = 0.15
    let weaponPercent = 0.15
    let playerViewSize = 3

    printf "Enter dungeon size: "
    let dungeonSize = int32 (Console.ReadLine())

    printf "Enter dungeon depth: "
    let dungeonDepth = int32 (Console.ReadLine())
    
    let generator = new Random (DateTime.Now.Millisecond)
    let playerPosition = [|generator.Next(1, dungeonSize); generator.Next(1, dungeonSize); 0|]

    let wallDungeon size depth (dungeon:cellState[,,]) =
            let adjustedSize = size
            for y = 0 to (depth - 1) do
                for i = 0 to adjustedSize do
                    dungeon.SetValue( {content = '#'; visible=true}, [|0; i; y|])
                    dungeon.SetValue( {content = '#'; visible=true}, [|adjustedSize; i; y|])
                    dungeon.SetValue( {content = '#'; visible=true}, [|i; 0; y;|])
                    dungeon.SetValue( {content = '#'; visible=true}, [|i; adjustedSize; y|])
            dungeon


    let buildDungeon size depth (playerPosition:int[]) =
        let generator = new Random (DateTime.Now.Millisecond)
        let emptyDungeon = Array3D.create (dungeonSize + 2) (dungeonSize + 2) depth {content='.'; visible=false}

        let walledDungeon = wallDungeon (size + 1) depth emptyDungeon

        let entrancePosition = playerPosition
        walledDungeon.SetValue({content='^'; visible=false}, entrancePosition)

        let rec generatePositions numToGenerate dungeonSize currentDepth (positions:List<(int * int)>) (dungeon:cellState[,,]) =
            if numToGenerate > 0 then
                let dimX, dimY = generator.Next(1, dungeonSize), generator.Next(1, dungeonSize)
                if not (List.exists (fun x -> (dimX, dimY) = x) positions) && (dungeon.[dimX, dimY, currentDepth] = {content='.'; visible=false}) then
                    let newPosition = (dimX, dimY)
                    let newList = List.Cons(newPosition, positions)
                    generatePositions (numToGenerate - 1) dungeonSize currentDepth newList dungeon
                else
                    generatePositions numToGenerate dungeonSize currentDepth positions dungeon
            else
                positions

        let addElementsToFloor symbol percent dungeonSize floorDepth (dungeon:cellState[,,]) =
            let elementNumbers = int32 (Math.Floor(percent * float (size * size)))

            let elementPositions = generatePositions elementNumbers dungeonSize floorDepth List.Empty dungeon
            for item in elementPositions do
                dungeon.SetValue({content=symbol;visible=false}, [|fst item; snd item; floorDepth|])

        let buildStairs upperFloorDepth lowerFloorDepth (dungeon:cellState[,,]) =
            let downStairsLocation = (generatePositions 1 dungeonSize upperFloorDepth List.Empty dungeon).Head
            dungeon.SetValue({content='▼';visible=false}, [|fst downStairsLocation; snd downStairsLocation; upperFloorDepth|])
            dungeon.SetValue({content='^';visible=false}, [|fst downStairsLocation; snd downStairsLocation; lowerFloorDepth|])


        
        for level = 0 to (depth - 1) do
            if (level < depth - 1) then
                buildStairs level (level + 1) walledDungeon

            addElementsToFloor '!' wumpusPercent (size + 1) level walledDungeon
            addElementsToFloor 'W' weaponPercent (size + 1) level walledDungeon
            addElementsToFloor 'P' pitTrapPercent (size + 1) level walledDungeon
            addElementsToFloor '$' goldPercent (size + 1) level walledDungeon

        walledDungeon
      
    let makeCellVisible (playerPosition:int[]) (dungeon:cellState[,,]) =
        let cellContent = dungeon.[playerPosition.[0],playerPosition.[1],playerPosition.[2]].content
        dungeon.SetValue({content=cellContent; visible=true}, playerPosition)

    let printPlayerView (playerPosition:int[]) (dungeon:cellState[,,]) =
        
        let constrainPositionToMin coord =
            match coord with 
            | x when (x - playerViewSize) < 0 -> 0
            | _ -> coord - playerViewSize

        let constrainPositionToMax coord =
            match coord with
            | x when (x + playerViewSize) > (dungeonSize + 1) -> dungeonSize + 1
            | _ -> coord + playerViewSize
               
        let minX = constrainPositionToMin playerPosition.[0]
        let maxX = constrainPositionToMax playerPosition.[0]
        let minY = constrainPositionToMin playerPosition.[1]
        let maxY = constrainPositionToMax playerPosition.[1]
        let tempMap = Array2D.init ((dungeon.GetUpperBound 0) + 1) ((dungeon.GetUpperBound 1) + 1) (fun x y ->  if dungeon.[x,y,playerPosition.[2]].visible then
                                                                                                                    dungeon.[x,y,playerPosition.[2]].content
                                                                                                                else
                                                                                                                    '?' )
        tempMap.SetValue('☻', playerPosition.[0..1])
        for i = 0 to tempMap.GetUpperBound 0 do
            for j = 0 to tempMap.GetUpperBound 1 do
                printf "%c" tempMap.[i,j]
            printfn ""


    let movePlayer direction (oldPosition:int[]) (dungeon:cellState[,,])=
        let newPosition =
            match direction with
                |'d' -> [|oldPosition.[0]; oldPosition.[1] + 1;oldPosition.[2]|]
                |'a' -> [|oldPosition.[0]; oldPosition.[1] - 1;oldPosition.[2]|]
                |'s' -> [|oldPosition.[0]+ 1; oldPosition.[1];oldPosition.[2]|]
                |'w' -> [|oldPosition.[0]- 1; oldPosition.[1];oldPosition.[2]|]
                | _ -> oldPosition
        if (dungeon.[newPosition.[0], newPosition.[1], oldPosition.[2]].content = '#') then
            oldPosition
        else
            newPosition


    let playerLoot (playerPos:int[]) (dungeon:cellState[,,]) =
        dungeon.SetValue({content='.'; visible=true}, playerPos.[0], playerPos.[1], playerPos.[2])

    let rec checkSurroundingsForChar (toCheck:list<char>) checkValue =
        if toCheck.IsEmpty then
            false
        else 
            let temp = toCheck.Head
            if temp = checkValue then
                true
            else
                checkSurroundingsForChar (toCheck.Tail) checkValue

    let rec printEnvironmentMessages (playerPosition:int[]) (dungeon:cellState[,,]) =
        let NESW = [    dungeon.[playerPosition.[0] + 1, playerPosition.[1],playerPosition.[2]].content;
                        dungeon.[playerPosition.[0] - 1, playerPosition.[1],playerPosition.[2]].content;
                        dungeon.[playerPosition.[0], playerPosition.[1] + 1,playerPosition.[2]].content;
                        dungeon.[playerPosition.[0], playerPosition.[1] - 1,playerPosition.[2]].content;]

        if checkSurroundingsForChar NESW '!' then
            printfn "You detect a foul stench in the air"
        if checkSurroundingsForChar NESW 'P' then
            printfn "You hear a howling wind"

    let rec gameLoop (playerPos:int[]) playerScore playerWeaponCount (dungeon:cellState[,,]) =
        printPlayerView playerPos dungeon
        printEnvironmentMessages playerPos dungeon
        let input = Console.ReadKey(true).KeyChar
        Console.Clear()
        match input with
            |'d' | 'a' | 's' | 'w' ->   let newPos = movePlayer input playerPos dungeon
                                        let unexploredRoom = (dungeon.[newPos.[0], newPos.[1], newPos.[2]].content = '?')
                                        makeCellVisible newPos dungeon
                                        match (dungeon.[newPos.[0], newPos.[1], newPos.[2]].content) with
                                        | '!' ->    if  (playerWeaponCount > 0) then
                                                        printfn "You slay the Wumpus"
                                                        printfn "You have %d weapons left" (playerWeaponCount - 1)
                                                        playerLoot newPos dungeon
                                                        gameLoop newPos (playerScore + 10) (playerWeaponCount - 1) dungeon
                                                    else
                                                        printfn "The Wumpus slays you"
                                        | 'P' ->    printfn "You fall down a pit. The reaper takes you."
                                        | '.' ->    if unexploredRoom then
                                                        gameLoop newPos (playerScore + 1) playerWeaponCount dungeon
                                                    else
                                                        gameLoop newPos playerScore playerWeaponCount dungeon
                                        | '$' ->    printfn "You spy a pile of gold in the room"
                                                    gameLoop newPos playerScore playerWeaponCount dungeon
                                        | 'W' ->    printfn "You spy a weapon in the room"
                                                    gameLoop newPos playerScore playerWeaponCount dungeon
                                        | _ ->      gameLoop newPos playerScore playerWeaponCount dungeon
            |'l' -> match (dungeon.[playerPos.[0], playerPos.[1], playerPos.[2]].content) with
                        |'W' -> playerLoot playerPos  dungeon
                                printfn "You now have %d weapons" (playerWeaponCount + 1)
                                gameLoop playerPos (playerScore + 5) (playerWeaponCount + 1) dungeon
                        |'$' -> playerLoot playerPos  dungeon
                                gameLoop playerPos (playerScore + 5) playerWeaponCount dungeon
                        |_ ->   printfn "You cannot loot that"
                                gameLoop playerPos playerScore playerWeaponCount dungeon
            |'r' -> match (dungeon.[playerPos.[0], playerPos.[1], playerPos.[2]].content) with
                    | '^' ->    if (playerPos.[2] = 0) then                     
                                    printfn "You escaped the Wumpus cave with %d points" playerScore
                                else
                                    printfn "You are now on basement level %d" (playerPos.[2] - 1)
                                    gameLoop [|playerPos.[0]; playerPos.[1]; playerPos.[2] - 1|] playerScore playerWeaponCount dungeon
                    | _ ->  printfn "You cannot excape from here"
                            gameLoop playerPos playerScore playerWeaponCount dungeon
            |'v' -> match (dungeon.[playerPos.[0], playerPos.[1], playerPos.[2]].content) with
                    | '▼' -> printfn "You are now on basement level %d" (playerPos.[2] + 1)
                             makeCellVisible [|playerPos.[0]; playerPos.[1]; playerPos.[2] + 1|] dungeon
                             gameLoop [|playerPos.[0]; playerPos.[1]; playerPos.[2] + 1|] (playerScore + 1) playerWeaponCount dungeon
                    | _ ->  printfn "You cannot go down from here"
                            gameLoop playerPos playerScore playerWeaponCount dungeon
            |'x' -> printfn "Thanks for playing"
            |'?' -> printfn "Handle printing commands"
                    gameLoop playerPos playerScore playerWeaponCount dungeon
            | _ ->  gameLoop playerPos playerScore playerWeaponCount dungeon


    let dungeon = buildDungeon dungeonSize dungeonDepth playerPosition
    makeCellVisible playerPosition dungeon
    gameLoop playerPosition 0 0 dungeon
    printf "\nPress any key to continue..."
    Console.ReadKey(true) |> ignore
    0 // return an integer exit code
