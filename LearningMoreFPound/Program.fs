// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.


open System
open System.Collections
open System.Text
open System.Threading
open System.Text.RegularExpressions
open System.Diagnostics
open System.Drawing
open Microsoft.FSharp.Reflection

[<Literal>]
let sacrificialLamb = 1
let spaceColor = Color.Black
let endColor = Color.Red
let startColor = Color.Pink
let wallColor = Color.Green
let pathColor = Color.Teal

[<EntryPoint>]
let main argv = 

    // Note: does not handle paths with halls wider than 1 x 1
    // Note: at some point, x and y became swapped; I don't want to unswap because I might break it
    let enableStepThrough = false

    let img = "Maze.bmp"

    let bitmaze = new System.Drawing.Bitmap(img);
    
    let getPixelColorOfImage xIndex yIndex (image:Bitmap) =
        image.GetPixel(xIndex, yIndex)

    let getMazeSymbolFromColor (color:Color) =
        match color.ToArgb() with
        | c when c = startColor.ToArgb() -> 'S'
        | c when c = endColor.ToArgb() -> 'E'
        | c when c = wallColor.ToArgb() -> '#'
        | c when c = spaceColor.ToArgb() -> ' '
        | _ -> '@'

    let getPixelRow heightIndex (bitmap:Bitmap) =
        Array.init bitmap.Width (fun index -> bitmap.GetPixel(index, heightIndex) |> getMazeSymbolFromColor)
        
    let maze = Array.init bitmaze.Height (fun index -> getPixelRow index bitmaze)

    let startCharacter = 'S'
    let endCharacter = 'E'
    let xDim = bitmaze.Width
    let yDim = bitmaze.Height

    let findCharacterInRow  (row:char[]) (desiredChar:char) =
        let index = Array.tryFindIndex (fun y -> y = desiredChar) row
        index

    let rec loopThroughRowsLookingForChar (maze:char[][]) currentRow desiredChar =
        if currentRow < yDim then
            let test = findCharacterInRow maze.[currentRow] desiredChar
            match test with
                | Some(test) -> [| currentRow; test;|]
                | _ -> loopThroughRowsLookingForChar maze (currentRow + 1) desiredChar
        else
            [|-1; -1|]

    let findCharacterInMaze (maze:char[][]) (desiredChar:char)=
        loopThroughRowsLookingForChar maze 0 desiredChar

    let startPoint = findCharacterInMaze maze startCharacter
    let endPoint = findCharacterInMaze maze endCharacter

    let getColorForCharacter value =
        match value with
        | ' ' | '$' | '@' -> Drawing.Color.Black
        | '*' -> Drawing.Color.White
        | 'E' -> Drawing.Color.Red
        | 'S' -> Drawing.Color.Pink
        | _ -> Drawing.Color.Green


    let printBoard (board:char[][]) =
        for item in board do
            for subItem in item do
                printf "%c" subItem
            printfn ""

    let printBoardWithPath (board:char[][]) =
        for item in board do
            for subItem in item do
                if subItem = '@' || subItem = '$' then
                    printf " "
                else
                    printf "%c" subItem
            printfn ""

    let drawBoard (board:char[][]) =
        let newBitmap = new System.Drawing.Bitmap(xDim, yDim)
        for i = 0 to xDim - 1 do
            for j = 0 to yDim - 1 do
                newBitmap.SetPixel(i, j, (getColorForCharacter board.[i].[j]))
        newBitmap.SetPixel(startPoint.[0], startPoint.[1], (getColorForCharacter 'S'))
        newBitmap.SetPixel(endPoint.[0], endPoint.[1], (getColorForCharacter 'E'))
        newBitmap.Save("SolvedMaze.bmp")

    let isValidTerrain (maze:char[][]) (position:int[]) =
        if (position.[0] >= 0) && (position.[1] >= 0) then
            if (position.[0] <= (maze.GetLength(0) - 1)) && (position.[1] <= (maze.[position.[0]].GetLength(0) - 1)) then
                match maze.[position.[0]].[position.[1]] with
                | ' ' | 'E' -> true
                | _ -> false
            else 
                false
        else 
            false

    let rec checkListForValidTerrain (maze:char[][]) (positions:List<int[]>) (valids:List<int[]>) =
        if positions.IsEmpty then
            valids
        else 
            if (isValidTerrain maze positions.Head) then
                maze.[positions.Head.[0]].SetValue('$', positions.Head.[1])
                checkListForValidTerrain maze positions.Tail (List.Cons(positions.Head, valids))
            else
                checkListForValidTerrain maze positions.Tail valids

    let getValidNeighbours (maze:char[][]) (playerPosition:int[]) =
        let potentialNeighbours = [    [|(playerPosition.[0] + 1); (playerPosition.[1]);|];
                                        [|(playerPosition.[0] - 1); (playerPosition.[1]);|];
                                        [|(playerPosition.[0]); (playerPosition.[1] - 1);|];
                                        [|(playerPosition.[0]); (playerPosition.[1] + 1);|]; ]
        checkListForValidTerrain maze potentialNeighbours List<int[]>.Empty


    let isPath (maze:char[][]) (position:int[]) =
        match maze.[position.[0]].[position.[1]] with
        | '*' -> true
        | _ -> false


    let rec checkListForPath (maze:char[][]) pathFound (positions:List<int[]>) =
        if positions.IsEmpty then
                false
        else
            if (positions.Head.[0] >= 0) && (positions.Head.[1] >= 0) && (isPath maze positions.Head) then 
                true
            else
                checkListForPath maze pathFound positions.Tail

    let checkForPath (maze:char[][]) (neighbours:List<int[]>) =
        checkListForPath maze 0 neighbours

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
        if (isValidTerrain maze playerPosition) then            
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

    let rec depthFirstSearch (maze:char[][]) (curPosition:int[]) (toVisit:System.Collections.Generic.Queue<int[]>) =
        let updatePosition (nMaze:char[][]) =
                        nMaze.[curPosition.[0]].SetValue('*', curPosition.[1])
                        nMaze
        
        if (not (curPosition = endPoint)) then    
            let newToVisits = getValidNeighbours maze curPosition
            if (not newToVisits.IsEmpty) then
                for item in newToVisits do
                    toVisit.Enqueue(item)
            maze.[curPosition.[0]].SetValue('@', curPosition.[1])
            if enableStepThrough then
                Console.Clear()
                printBoard maze
                Console.ReadKey (false) |> ignore
            if (toVisit.Count > 0) then
                let nextPosition = toVisit.Dequeue()
                let updatedMaze = depthFirstSearch maze nextPosition toVisit
                if (checkForPath updatedMaze newToVisits) then //check if one of neighbours has a * in it                    
                    let tmep = updatePosition updatedMaze
                    if enableStepThrough then
                        Console.Clear()
                        printBoard tmep
                        Console.ReadKey (false) |> ignore
                    tmep
                else
                    updatedMaze
            else
                maze
        else
            updatePosition maze

    let finalMaze = depthFirstSearch maze startPoint (new System.Collections.Generic.Queue<int[]>())
    drawBoard finalMaze


    printf "\nPress any key to continue..."
    Console.ReadKey(true) |> ignore
    0 // return an integer exit code
