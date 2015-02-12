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

    printf "Enter dungeon size: "
    let dungeonSize = int32 (Console.ReadLine())

    printf "Enter dungeon depth: "
    let dungeonDepth = int32 (Console.ReadLine())
    
    let generator = new Random (DateTime.Now.Millisecond)
    let playerPosition = [|generator.Next(1, dungeonSize); generator.Next(1, dungeonSize); 0|]
      
    let makeCellVisible (playerPosition:int[]) (dungeon:DataTypes.cellState[,,]) =
        let cellContent = dungeon.[playerPosition.[0],playerPosition.[1],playerPosition.[2]].content
        dungeon.SetValue( new DataTypes.cellState(cellContent, true), playerPosition)

    let printPlayerView (playerPosition:int[]) (dungeon:DataTypes.cellState[,,]) =
        
        let constrainPositionToMin coord =
            match coord with 
            | x when (x - DungeonConfig.playerViewSize) < 0 -> 0
            | _ -> coord - DungeonConfig.playerViewSize

        let constrainPositionToMax coord =
            match coord with
            | x when (x + DungeonConfig.playerViewSize) > (dungeonSize + 1) -> dungeonSize + 1
            | _ -> coord + DungeonConfig.playerViewSize
               
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


    let movePlayer direction (oldPosition:int[]) (dungeon:DataTypes.cellState[,,])=
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


    let playerLoot (playerPos:int[]) (dungeon:DataTypes.cellState[,,]) =
        dungeon.SetValue(new DataTypes.cellState('.', true), playerPos.[0], playerPos.[1], playerPos.[2])

    let rec checkSurroundingsForChar (toCheck:list<char>) checkValue =
        if toCheck.IsEmpty then
            false
        else 
            let temp = toCheck.Head
            if temp = checkValue then
                true
            else
                checkSurroundingsForChar (toCheck.Tail) checkValue

    let rec printEnvironmentMessages (playerPosition:int[]) (dungeon:DataTypes.cellState[,,]) =
        let NESW = [    dungeon.[playerPosition.[0] + 1, playerPosition.[1],playerPosition.[2]].content;
                        dungeon.[playerPosition.[0] - 1, playerPosition.[1],playerPosition.[2]].content;
                        dungeon.[playerPosition.[0], playerPosition.[1] + 1,playerPosition.[2]].content;
                        dungeon.[playerPosition.[0], playerPosition.[1] - 1,playerPosition.[2]].content;]

        if checkSurroundingsForChar NESW '!' then
            printfn "You detect a foul stench in the air"
        if checkSurroundingsForChar NESW 'P' then
            printfn "You hear a howling wind"

    let rec gameLoop (playerPos:int[]) playerScore playerWeaponCount (dungeon:DataTypes.cellState[,,]) =
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


    let dungeon = DungeonBuilder.buildDungeon dungeonSize dungeonDepth playerPosition
    makeCellVisible playerPosition dungeon
    gameLoop playerPosition 0 0 dungeon
    printf "\nPress any key to continue..."
    Console.ReadKey(true) |> ignore
    0 // return an integer exit code
