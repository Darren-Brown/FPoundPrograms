// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.


open System
open System.IO
open System.Drawing
open System.Collections
open System.Text
open System.Threading
open System.Text.RegularExpressions
open System.Diagnostics
open Microsoft.FSharp.Reflection

[<EntryPoint>]
let main argv = 
    let rand = new System.Random ()

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

    printf "Please enter direction string size:"
    let inputSize = Int32.Parse(Console.ReadLine())
    let generateDirection inputVal=
        match inputVal with
        | 0 -> 'L'
        | 1 -> 'R'
        | _ -> 'E'         

    printf "Please enter area size:"
    let areaSize = Int32.Parse(Console.ReadLine())

    let colorIncrement = 255 / inputSize
    let colorArray = Array.init (inputSize) (fun index -> Color.FromArgb((index * colorIncrement), (Math.Max(0, (index * colorIncrement - 150))), (index * colorIncrement)))
    
    let initialPosition = [|(areaSize/2); (areaSize/2)|]

    let printBoardImage (maze:int[,]) =
        let test = new Bitmap(areaSize, areaSize)
//        let testCC = (enum<ConsoleColor>(board.[0,0]))
//        let test2 = testCC.ToString()
//        let testC = Color.FromName test2
//        //use wr = new StreamWriter ("maze.txt", false)
//        //let sizeString = (areaSize + 2).ToString() + " " + (ySize + 2).ToString()
//        //wr.WriteLine (sizeString)
        for i = 0 to (areaSize - 1) do
            for j = 0 to (areaSize - 1) do
//                test.SetPixel(i, j, (Color.FromName ((enum<ConsoleColor>(maze.[i,j])).ToString())))
                test.SetPixel(i, j, colorArray.[maze.[i,j]])
        
        test.Save("daBoard.bmp", System.Drawing.Imaging.ImageFormat.Bmp)

    let turnAnt currentDirection turnDirection =
        match turnDirection with
        | 'L' ->    Math.Abs(((currentDirection - 1) + 4) % 4)
        | 'R' ->    Math.Abs(((currentDirection + 1) + 4) % 4)
        | _ ->      currentDirection

    let getNewAntPosition (curPosition:int[]) direction =
        match direction with
        //north
        | 0 ->  let newY = curPosition.[1] - 1
                [| curPosition.[0]; Math.Abs(((newY) + areaSize) % areaSize);|]
        //east
        | 1 ->  let newX = curPosition.[0] + 1
                [| Math.Abs(((newX) + areaSize) % areaSize); curPosition.[1];|]
        //south
        | 2 ->  let newY = curPosition.[1] + 1
                [| curPosition.[0]; Math.Abs(((newY) + areaSize) % areaSize);|]     
        // west
        | 3 ->  let newX = curPosition.[0] - 1
                [| Math.Abs(((newX) + areaSize) % areaSize); curPosition.[1];|]
        | _ -> curPosition
    

    

    let rec mainLoop patternSize printAtInterval terminateAtInterval=

        let rec runAnt (directionArray:char[]) (antPos:int[]) antDir (board:int[,]) printCountdown terminateCountdown =
            let newDirection = turnAnt antDir (directionArray.[board.[antPos.[0], antPos.[1]]])
            let newPosition = getNewAntPosition antPos newDirection
            board.SetValue ((board.[antPos.[0], antPos.[1]] + 1) % (directionArray.Length), antPos)
        
            if printCountdown < 1 then
                printfn "began printing board at %s" (DateTime.Now.ToString())
                printBoardImage board
                printfn "new board generated at %s" (DateTime.Now.ToString())
                Thread.Sleep(2000)
                if terminateCountdown < 1 then
                    printfn "finished pattern %A" directionArray
                    Thread.Sleep(5000)
                else
                    runAnt directionArray newPosition newDirection board printAtInterval (terminateCountdown - 1)
            else
                runAnt directionArray newPosition newDirection board (printCountdown - 1) terminateCountdown          

        let inputDirectionArray = Array.init patternSize (fun _ -> rand.Next(0, 2) |> generateDirection)
        let board = Array2D.init areaSize areaSize (fun _ _ -> 0)
        runAnt inputDirectionArray initialPosition 0 board printAtInterval terminateAtInterval
        mainLoop patternSize printAtInterval terminateAtInterval


   // printMazeImage board
    mainLoop inputSize 20000 15
    printf "\nPress any key to continue..."
    Console.ReadKey(true) |> ignore
    0 // return an integer exit code
