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
    
    let generator = new Random (DateTime.Now.Millisecond)
    let playerPosition = [|generator.Next(1, dungeonSize); generator.Next(1, dungeonSize);|]

    let wallDungeon size (dungeon:cellState[,]) =
            let adjustedSize = size
            for i = 0 to adjustedSize do
                dungeon.SetValue( {content = '#'; visible=true}, [|0; i;|])
                dungeon.SetValue( {content = '#'; visible=true}, [|adjustedSize; i;|])
                dungeon.SetValue( {content = '#'; visible=true}, [|i; 0;|])
                dungeon.SetValue( {content = '#'; visible=true}, [|i; adjustedSize;|])
            dungeon

    let buildDungeon size (playerPosition:int[]) =
        let generator = new Random (DateTime.Now.Millisecond)
        let emptyDungeon = Array2D.create (dungeonSize + 2) (dungeonSize + 2) {content='.'; visible=false}

        let walledDungeon = wallDungeon (size + 1) emptyDungeon

        let entrancePosition = playerPosition
        walledDungeon.SetValue({content='^'; visible=false}, entrancePosition)

        let addElements symbol percent dungeonSize (dungeon:cellState[,]) =
            let elementNumbers = int32 (Math.Floor(percent * float (size * size)))
            let rec generatePositions numToGenerate dungeonSize (positions:List<(int * int)>) (dungeon:cellState[,]) =
                if numToGenerate > 0 then
                    let dimX, dimY = generator.Next(1, dungeonSize), generator.Next(1, dungeonSize)
                    if not (List.exists (fun x -> (dimX, dimY) = x) positions) && (dungeon.[dimX, dimY] = {content='.'; visible=false}) then
                        let newPosition = (dimX, dimY)
                        let newList = List.Cons(newPosition, positions)
                        generatePositions (numToGenerate - 1) dungeonSize newList dungeon
                    else
                        generatePositions numToGenerate dungeonSize positions dungeon
                else
                    positions

            let elementPositions = generatePositions elementNumbers dungeonSize List.Empty dungeon
            for item in elementPositions do
                dungeon.SetValue({content=symbol;visible=false}, [|fst item; snd item|])

        addElements '!' wumpusPercent (size + 1) walledDungeon
        addElements 'W' weaponPercent (size + 1) walledDungeon
        addElements 'P' pitTrapPercent (size + 1) walledDungeon
        addElements '$' goldPercent (size + 1) walledDungeon

        walledDungeon
      
    let makeCellVisible (playerPosition:int[]) (dungeon:cellState[,]) =
        let cellContent = dungeon.[playerPosition.[0],playerPosition.[1]].content
        dungeon.SetValue({content=cellContent; visible=true}, playerPosition)

    let printPlayerView (playerPosition:int[]) (dungeon:cellState[,]) =
        
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
        let tempMap = Array2D.init ((dungeon.GetUpperBound 0) + 1) ((dungeon.GetUpperBound 1) + 1) (fun x y ->  if dungeon.[x,y].visible then
                                                                                                                    dungeon.[x,y].content
                                                                                                                else
                                                                                                                    '?' )
        tempMap.SetValue('☻', playerPosition)
        for i = 0 to tempMap.GetUpperBound 0 do
            for j = 0 to tempMap.GetUpperBound 1 do
                printf "%c" tempMap.[i,j]
            printfn ""


    let movePlayer direction (oldPosition:int[]) (dungeon:cellState[,])=
        let newPosition =
            match direction with
                |'d' -> [|oldPosition.[0]; oldPosition.[1] + 1;|]
                |'a' -> [|oldPosition.[0]; oldPosition.[1] - 1;|]
                |'s' -> [|oldPosition.[0]+ 1; oldPosition.[1];|]
                |'w' -> [|oldPosition.[0]- 1; oldPosition.[1];|]
                | _ -> oldPosition
        if (dungeon.[newPosition.[0], newPosition.[1]].content = '#') then
            oldPosition
        else
            newPosition

    let dungeon = buildDungeon dungeonSize playerPosition

    let obliterateWeapons (dungeon:cellState[,])=
        for i = 0 to dungeonSize do
            for j = 0 to dungeonSize do
                if dungeon.[i,j].content = 'W' then
                    let tempVisible = dungeon.[i,j]
                    dungeon.SetValue({content='$'; visible=tempVisible.visible}, [|i; j;|])


    let playerLoot (playerPos:int[]) (dungeon:cellState[,]) =
        dungeon.SetValue({content='.'; visible=true}, playerPos.[0], playerPos.[1])

    let rec checkSurroundingsForChar (toCheck:list<char>) checkValue =
        if toCheck.IsEmpty then
            false
        else 
            let temp = toCheck.Head
            if temp = checkValue then
                true
            else
                checkSurroundingsForChar (toCheck.Tail) checkValue

    let rec printEnvironmentMessages (playerPosition:int[]) (dungeon:cellState[,]) =
        let NESW = [    dungeon.[playerPosition.[0] + 1, playerPosition.[1]].content;
                        dungeon.[playerPosition.[0] - 1, playerPosition.[1]].content;
                        dungeon.[playerPosition.[0], playerPosition.[1] + 1].content;
                        dungeon.[playerPosition.[0], playerPosition.[1] - 1].content;]

        if checkSurroundingsForChar NESW '!' then
            printfn "You detect a foul stench in the air"
        if checkSurroundingsForChar NESW 'P' then
            printfn "You hear a howling wind"

    let rec gameLoop (playerPos:int[]) playerScore playerArmed (dungeon:cellState[,]) =
        printPlayerView playerPos dungeon
        printEnvironmentMessages playerPos dungeon
        let input = Console.ReadKey(true).KeyChar
        Console.Clear()
        match input with
            |'d' | 'a' | 's' | 'w' ->   let newPos = movePlayer input playerPos dungeon
                                        let unexploredRoom = (dungeon.[newPos.[0], newPos.[1]].content = '?')
                                        makeCellVisible newPos dungeon
                                        match (dungeon.[newPos.[0], newPos.[1]].content) with
                                        | '!' ->    if  playerArmed then
                                                        printfn "You slay the Wumpus"
                                                        playerLoot newPos dungeon
                                                        gameLoop newPos (playerScore + 10) playerArmed dungeon
                                                    else
                                                        printfn "The Wumpus slays you"
                                        | 'P' ->    printfn "You fall down a pit. The reaper takes you."
                                        | '.' ->    if unexploredRoom then
                                                        gameLoop newPos (playerScore + 1) playerArmed dungeon
                                                    else
                                                        gameLoop newPos playerScore playerArmed dungeon
                                        | '$' ->    printfn "You spy a pile of gold in the room"
                                                    gameLoop newPos playerScore playerArmed dungeon
                                        | 'W' ->    printfn "You spy a weapon in the room"
                                                    gameLoop newPos playerScore playerArmed dungeon
                                        | _ ->      gameLoop newPos playerScore playerArmed dungeon
            |'l' -> match (dungeon.[playerPos.[0], playerPos.[1]].content) with
                        |'W' -> playerLoot playerPos  dungeon
                                obliterateWeapons dungeon
                                gameLoop playerPos (playerScore + 5) true dungeon
                        |'$' -> playerLoot playerPos  dungeon
                                gameLoop playerPos (playerScore + 5) playerArmed dungeon
                        |_ ->   printfn "You cannot loot that"
                                gameLoop playerPos playerScore playerArmed dungeon
            |'r' -> match (dungeon.[playerPos.[0], playerPos.[1]].content) with
                    | '^' -> printfn "You escaped the Wumpus cave with %d points" playerScore
                    | _ ->  printfn "You cannot excape from here"
                            gameLoop playerPos playerScore playerArmed dungeon
            |'x' -> printfn "Thanks for playing"
            |'?' -> printfn "Handle printing commands"
                    gameLoop playerPos playerScore playerArmed dungeon
            | _ ->  gameLoop playerPos playerScore playerArmed dungeon

    makeCellVisible playerPosition dungeon
    gameLoop playerPosition 0 false dungeon
    printf "\nPress any key to continue..."
    Console.ReadKey(true) |> ignore
    0 // return an integer exit code
