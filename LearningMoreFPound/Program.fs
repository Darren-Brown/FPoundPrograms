// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open System.IO
open System
open System.Collections
open System.Text
open System.Threading
open System.Text.RegularExpressions
open System.Diagnostics
open Microsoft.FSharp.Reflection

type CellStatus = Empty | Potential | Full | Wall

#nowarn "40"
[<EntryPoint>]
let main argv = 
    let rnd = System.Random()

    printf "Enter maze X dimension:"
    let xSize = Int32.Parse(Console.ReadLine()) * 2 - 1
    printf "Enter maze Y dimension:"
    let ySize = Int32.Parse(Console.ReadLine()) * 2 - 1
    let initialPosition = [|1; 1|]

    let buildEmptyMaze xDim yDim =
        Array2D.init (xDim + 2) (yDim + 2) (fun xIndex yIndex ->  match xIndex with
                                                                    | 0  ->  '#'
                                                                    | outside when outside = (xSize + 1) -> '#'
                                                                    | 1 ->  match yIndex with
                                                                            | 1 -> '*'
                                                                            | _ ->  match yIndex with
                                                                                    | 0 -> '#'
                                                                                    | outside when outside = (ySize + 1) -> '#'
                                                                                    | _ -> ' '
                                                                    | _ ->  match yIndex with
                                                                            | 0 -> '#'
                                                                            | outside when outside = (ySize + 1) -> '#'
                                                                            | _ -> ' ')

    let rec findPath (maze:char[,]) (position:int[]) (oldPos:int[])=
//        Console.Clear()
//        printfn "%A" maze
//        Console.ReadKey(true) |> ignore
        let rec getRndDirection (position:int[]) (oldPos:int[]) (maze:char[,])=
            let rndDirection = rnd.Next(1, 5)
            let rndPosition =
                match rndDirection with
                | 1 -> [|(position.[0] + 2); (position.[1])|]
                | 2 -> [|(position.[0] - 2); (position.[1])|]
                | 3 -> [|(position.[0]); (position.[1] + 2)|]
                | 4 -> [|(position.[0] ); (position.[1] - 2)|]
                | _ -> position

            let isPositionWithinBounds (position:int[]) =
                (position.[0] >= 1) 
                && (position.[0] <= ((maze.GetUpperBound 0) - 1)) 
                && (position.[1] >= 1) 
                && (position.[1] <= ((maze.GetUpperBound 1) - 1))

            if isPositionWithinBounds rndPosition && rndPosition <> oldPos then
                rndPosition
            else
                getRndDirection position oldPos maze
        
        let getIntermediate (newPos:int[]) (oldPos:int[]) =
            let xDif = (newPos.[0] - oldPos.[0]) / 2
            let yDif = (newPos.[1] - oldPos.[1]) / 2
            [| (oldPos.[0] + xDif); (oldPos.[1] + yDif);|]

        let positionStatus (curPos:int[])=
            match maze.[curPos.[0], curPos.[1]] with
            | ' ' -> Empty
            | '#' -> Wall
            | '*' -> Full
            | '@' -> Potential
            | _ -> Wall

        let newPos = getRndDirection position oldPos maze
        let newPosStatus = positionStatus newPos
        let intermediate = getIntermediate newPos position

        match newPosStatus with
        | Empty ->  //TODO: check if end first
                    maze.SetValue('@', newPos)
                    maze.SetValue('@', intermediate)
                    findPath maze newPos position
//                    Console.Clear()
//                    printfn "%A" maze
//                    Console.ReadKey(true) |> ignore
                    let updatedPosStatus = positionStatus position
                    match updatedPosStatus with
                    | Empty ->  maze.SetValue(' ', intermediate)
                                maze.SetValue('@', position)
                                findPath maze position oldPos
                    | Full ->   maze.SetValue('*', intermediate)
                                //path complete, move to next available cell
                    | _ ->  //still have to unwind
                            let updatedNewPosStatus = positionStatus newPos
                            match updatedNewPosStatus with
                            | Full ->   maze.SetValue('*', intermediate)
                                        maze.SetValue('*', position)
                            | Empty ->  maze.SetValue(' ', position)
                                        maze.SetValue(' ', intermediate)
                            | _ ->  //should not happen
                                    printfn "something bad happened coming back from %A to %A" newPos position
        | Potential ->  maze.SetValue(' ', newPos)
                        maze.SetValue(' ', intermediate)
                        maze.SetValue(' ', position)
        | Full ->   maze.SetValue('*', newPos)
                    maze.SetValue('*', intermediate)
                    maze.SetValue('*', position)
        | Wall ->   //try again
                    findPath maze position oldPos

    let tempMaze = buildEmptyMaze xSize ySize
    findPath tempMaze [|1; 1;|] [|0; 0;|]
    for i = 1 to xSize do
        if i % 2 = 1 then
            for j = 1 to ySize do
                if j % 2 = 1 then
                    if tempMaze.[i, j] <> '*' then
                        tempMaze.SetValue( '@', [|i; j;|])
                        findPath tempMaze [|i; j;|] [|0; 0;|]

    for i = 1 to xSize do
        for j = 1 to ySize do
            let replaceValue = 
                match tempMaze.[i,j] with
                | '*' -> ' '
                | _ -> '#'
            tempMaze.SetValue(replaceValue, [|i;j;|])

    tempMaze.SetValue('S', [|1;1;|])
    tempMaze.SetValue('E', [|xSize;ySize;|])

    //printfn "%A" tempMaze
    let printMaze (maze:char[,]) =
        use wr = new StreamWriter ("maze.txt", false)
        let sizeString = (xSize + 2).ToString() + " " + (ySize + 2).ToString()
        wr.WriteLine (sizeString)
        for i = 0 to xSize + 1 do
            for j = 0 to ySize + 1 do
                wr.Write(maze.[i,j])
                printf "%c" maze.[i,j]
            printfn ""
            wr.WriteLine()


    let getColorForCharacter value =
        match value with
        | ' ' -> Drawing.Color.Black
        | 'E' -> Drawing.Color.Red
        | 'S' -> Drawing.Color.Pink
        | _ -> Drawing.Color.Green

    let drawMaze (maze:char[,])=
        let bitmap = new System.Drawing.Bitmap(xSize + 2, ySize + 2)
        for i = 0 to xSize + 1 do
            for j = 0 to ySize + 1 do
                bitmap.SetPixel(i, j, (getColorForCharacter maze.[i,j]))
        bitmap.Save("Maze.bmp")


    //printMaze tempMaze
    drawMaze tempMaze
    printf "\nPress any key to continue..."
    Console.ReadKey(true) |> ignore
    0 // return an integer exit code
