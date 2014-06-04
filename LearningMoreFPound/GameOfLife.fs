module GameOfLife

open System
open System.Text
open System.Threading
open System.Text.RegularExpressions
open System.Diagnostics
open Microsoft.FSharp.Reflection

let controlData = Console.ReadLine().Split[|' '|]
let iterations = int32 controlData.[0]
let xDim = int32 controlData.[1]
let yDim = int32 controlData.[2]

let currentBoard = [| for i in 0 .. yDim - 1 -> (Console.ReadLine().ToCharArray())|]

let incrementIfAlive (board:char [][]) row column =
    if board.[row].[column].Equals('#') then
        1
    else
        0

let countLivingInRowTriplet board row column excludeCenter=
    let columnPlusOne = (column + 1) % xDim
    let columnMinusOne = (column - 1 + xDim) % xDim
    if excludeCenter then
        (incrementIfAlive board row columnPlusOne) + (incrementIfAlive board row columnMinusOne)
    else
        (incrementIfAlive board row columnPlusOne) + (incrementIfAlive board row columnMinusOne) + (incrementIfAlive board row column)

let countLivingNeighbours board row column =
    let startRow = (row - 1 + yDim) % yDim
    let endRow = (row + 1) % yDim
    (countLivingInRowTriplet board startRow column false) + (countLivingInRowTriplet board row column true) + (countLivingInRowTriplet board endRow column false)

let isCellAlive (board:char[][]) row column =
    let livingNeighbourCount = (countLivingNeighbours board row column)
    if livingNeighbourCount = 3 then
        true
    else if (livingNeighbourCount = 2) && board.[row].[column] = '#' then
            true
        else
            false 
        
let getCellCharacter board row column =
    if isCellAlive board row column then
        '#'
    else
        '.'

let generateNewRow board row =
    [|for i in 0 .. xDim - 1 -> getCellCharacter board row i|]

let generateNewBoard oldBoard =
    [|for i in 0 .. yDim - 1 -> generateNewRow oldBoard i|]

let printBoard (board:char[][]) =
    for item in board do
        for subItem in item do
            printf "%c" subItem
        printfn ""
    
printfn ""
let rec gameLoop (board:char[][]) iterationsRemaining =
    if iterationsRemaining > 0 then
        let stopwatch = Stopwatch.StartNew()
        let newBoard = generateNewBoard board
        stopwatch.Stop |> ignore
        Console.Clear ()
            
        printfn  "%s" (stopwatch.ElapsedTicks.ToString())
        printBoard newBoard
        Console.ReadKey(true) |> ignore
        gameLoop newBoard (iterationsRemaining - 1)            
    else
        printfn ""

gameLoop currentBoard iterations