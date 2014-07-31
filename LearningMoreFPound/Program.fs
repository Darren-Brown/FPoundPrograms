// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.


open System
open System.Collections
open System.Text
open System.Threading
open System.Text.RegularExpressions
open System.Diagnostics
open Microsoft.FSharp.Reflection

[<EntryPoint>]
let main argv = 

    /// Colored printf
    let cprintf c fmt = 

        Printf.kprintf 
            (fun s -> 
                let old = System.Console.ForegroundColor 
                try 
                  System.Console.ForegroundColor <- c;
                  System.Console.Write s
                finally
                  System.Console.ForegroundColor <- old) 
            fmt
        
    // Colored printfn
    let cprintfn c fmt = 
        cprintf c fmt
        printfn ""

    printf "Please enter direction string:"
    let inputDirectionArray = (Console.ReadLine ()).ToCharArray()
    printf "Please enter area size:"
    let areaSize = Int32.Parse(Console.ReadLine())

    //Create color/direction mapping
//    let colorArray = (ConsoleColor.GetValues (typeof<ConsoleColor>))
//    let tempArray = [| for i in 0 .. (inputDirectionArray.Length - 1) -> i |]
//
//    let mapColor (index:int) =
//        colorArray.GetValue(index)
//
//    let mappedColorArray = tempArray |> Array.map mapColor
    //let colorDirectionArray = Array.zip mappedColorArray inputDirectionArray
     
    //Initialize Board
    let board = Array2D.init areaSize areaSize (fun _ _ -> 0)
    let initialPosition = [|(areaSize/2); (areaSize/2)|]
    


    let printBoard (board:int[,]) =
        for i = 0 to (areaSize - 1) do
            for j = 0 to (areaSize - 1) do
                cprintf (enum<ConsoleColor>(board.[i,j])) "#"
            printfn ""

    let turnAnt currentDirection turnDirection =
        match turnDirection with
        | 'L' ->    let newdir = currentDirection - 1
                    if newdir < 0 then
                        3
                    else
                        newdir
        | 'R' ->    (currentDirection + 1) % 4
        | _ ->      currentDirection

    let getNewAntPosition (curPosition:int[]) direction =
        match direction with
        //north
        | 0 ->  let newY = curPosition.[1] - 1
                if newY < 0 then
                    [| curPosition.[0]; (areaSize - 1);|]
                else
                    [| curPosition.[0]; newY;|]
        //east
        | 1 ->  let newX = curPosition.[0] + 1
                if newX = areaSize then
                    [|0; curPosition.[1]|]
                else
                    [|newX; curPosition.[1]|]
        //south
        | 2 ->  let newY = curPosition.[1] + 1
                if newY = areaSize then
                    [| curPosition.[0]; 0;|]
                else
                    [| curPosition.[0]; newY;|]     
        // west
        | 3 ->  let newX = curPosition.[0] - 1
                if newX < 0 then
                    [|(areaSize - 1); curPosition.[1]|]
                else
                    [|newX; curPosition.[1]|]
        | _ -> curPosition

    let rec mainLoop (antPosition:int[]) antDirection (board:int[,]) count=

        let newDirection = turnAnt antDirection (inputDirectionArray.[board.[antPosition.[0], antPosition.[1]]])
        let newPosition = getNewAntPosition antPosition newDirection
        board.SetValue ((board.[antPosition.[0], antPosition.[1]] + 1) % (inputDirectionArray.Length), antPosition)
        
        Console.Clear()
        printBoard board
        Thread.Sleep(50)
        mainLoop newPosition newDirection board 0
//        if count > 1 then
//            Console.Clear()
//            printBoard board
//            Thread.Sleep(100)
//            mainLoop newPosition newDirection board 0
//        else
//            mainLoop newPosition newDirection board (count + 1)

    mainLoop initialPosition 0 board 0
    printf "\nPress any key to continue..."
    Console.ReadKey(true) |> ignore
    0 // return an integer exit code
