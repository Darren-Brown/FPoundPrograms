// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.


open System
open System.Text
open System.Threading
open System.Text.RegularExpressions
open System.Diagnostics
open Microsoft.FSharp.Reflection

[<EntryPoint>]
let main argv = 

    let startCharacter = 'S'
    let endCharacter = 'E'
    let controlData = Console.ReadLine().Split[|' '|]
    let xDim = int32 controlData.[0]
    let yDim = int32 controlData.[1]

    let maze = [| for i in 0 .. yDim - 1 -> (Console.ReadLine().ToCharArray())|]

    let findCharacterInRow  (row:char[]) (desiredChar:char) =
        let index = Array.tryFindIndex (fun y -> y = desiredChar) row
        index

    let rec loopThroughRowsLookingForChar (maze:char[][]) currentRow desiredChar =
        if currentRow < yDim then
            let test = findCharacterInRow maze.[currentRow] desiredChar
            match test with
                | Some(test) -> [| test; currentRow;|]
                | _ -> loopThroughRowsLookingForChar maze (currentRow + 1) desiredChar
        else
            [|-1; -1|]

    let findCharacterInMaze (maze:char[][]) (desiredChar:char)=
        loopThroughRowsLookingForChar maze 0 desiredChar


    let printBoard (board:char[][]) =
        for item in board do
            for subItem in item do
                printf "%c" subItem
            printfn ""

    let startPoint = findCharacterInMaze maze startCharacter
    let endPoint = findCharacterInMaze maze endCharacter

    let isPlayerAtEnd (playerPosition:int[]) (endPosition:int[]) =
        if playerPosition.[0] = endPosition.[0] && playerPosition.[1] = endPosition.[1] then
            true
        else
            false

    let updatePlayerPositionInMaze (maze:char[][]) (newPosition:int[]) (playerPosition:int[]) =
        maze.[playerPosition.[0]].SetValue(' ', playerPosition.[1])
        maze.[newPosition.[0]].SetValue('S', newPosition.[1])
        maze

    let tryMovePlayer (maze:char[][]) direction (playerPosition:int[])=
        let newPosition =
            match direction with
                |'d' -> [|playerPosition.[0]; playerPosition.[1] + 1;|]
                |'a' -> [|playerPosition.[0]; playerPosition.[1] - 1;|]
                |'s' -> [|playerPosition.[0]+ 1; playerPosition.[1];|]
                |'w' -> [|playerPosition.[0]- 1; playerPosition.[1];|]
                | _ -> playerPosition
        //can probably break this into a "isValidTerrain" function if I introduce more tiles
        if maze.[newPosition.[0]].[newPosition.[1]] = ' ' || maze.[newPosition.[0]].[newPosition.[1]] = endCharacter then            
            newPosition
        else
            playerPosition

    let rec gameLoop (currentMaze:char[][]) (playerPosition:int[])=
        if (isPlayerAtEnd playerPosition endPoint) then
            printfn "You Win"
        else 
            let (desiredDirection:char) = Console.ReadKey().KeyChar
            Console.Clear()
            let newPlayerPosition = tryMovePlayer maze desiredDirection playerPosition
            let newMaze = updatePlayerPositionInMaze maze newPlayerPosition playerPosition
        
            printBoard newMaze
            gameLoop newMaze newPlayerPosition

    gameLoop maze startPoint

    printf "\nPress any key to continue..."
    Console.ReadKey(true) |> ignore
    0 // return an integer exit code
