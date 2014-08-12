// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.


open System
open System.IO
open System.Windows.Forms
open System.ComponentModel
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

//    printf "Please enter direction string size:"
    //let inputSize = Math.Min(Int32.Parse(Console.ReadLine()), 111765)
    let inputSize = 4080
    let generateDirection inputVal=
        match inputVal with
        | 0 -> 'L'
        | 1 -> 'R'
        | _ -> 'E'         

//    printf "Please enter area size:"
    //let areaSize = Int32.Parse(Console.ReadLine())

    // for background
//    let xSize = 1920
//    let ySize = 1080
    let xSize = 3840
    let ySize = 2160

    let colorIncrement = Math.Max(1, 255 / inputSize)
    //let colorArray = Array.init (inputSize) (fun index -> Color.FromArgb(index % 255, (index / 255) % 255, (index / 65025) % 255))
    let colorFunction index = Color.FromArgb((index / 16) % 255, ( index / 4) % 255, ( index % 255))
    //let colorArray = Array.init (inputSize) (fun index -> Color.FromArgb(Math.Min(255, (index * colorIncrement)), (Math.Min(255, Math.Max(0, (index - 100) * colorIncrement))), Math.Min(50, (colorIncrement * index)/6)))
    //let colorArray = Array.init (inputSize) (fun index -> Color.FromArgb( (Math.Min(255, Math.Max(0, (index - (2*inputSize/3)) * colorIncrement))), (Math.Min(255, Math.Max(0, (index - (inputSize/2)) * colorIncrement))), Math.Min(255, (index * colorIncrement))))
    //let colorArray = Array.init (inputSize) (fun index -> Color.FromArgb( (Math.Min(200, Math.Max(0, (index - (2*inputSize/3)) * colorIncrement))), (Math.Min(200, Math.Max(0, (index - (inputSize/2)) * colorIncrement))), Math.Min(200, (index * colorIncrement))))
    
    //    let colorArray = Array.init (inputSize) (fun index -> Color.FromArgb((Math.Min(255, index * colorIncrement)), (Math.Min(255, Math.Max( 0, (index - 510) * colorIncrement))), (Math.Min(255, Math.Max( 0, (index - 255) * colorIncrement)))))

    let initialPosition = [|(xSize/2); (ySize/2)|]
//    let initialPosition = [|0;0|]
    let printBoardImage (maze:int[,]) = async{
        printfn "generating bitmap at %s" (DateTime.Now.ToString())
        let test = new Bitmap(xSize, ySize)
        for i = 0 to (xSize - 1) do
            for j = 0 to (ySize - 1) do
//                test.SetPixel(i, j, (Color.FromName ((enum<ConsoleColor>(maze.[i,j])).ToString())))
                test.SetPixel(i, j, (colorFunction maze.[i,j]))
        
        File.Delete("daBoardOld2.bmp")
        File.Move("daBoardOld.bmp", "daBoardOld2.bmp")
        File.Move("daBoard.bmp", "daBoardOld.bmp")
        
        test.Save("daBoard.bmp", System.Drawing.Imaging.ImageFormat.Bmp) 
        printfn "Printed board at %s" (DateTime.Now.ToString())}

    let turnAnt currentDirection turnDirection =
        match turnDirection with
        | 'L' ->    (currentDirection + 3) % 4
        | 'R' ->    (currentDirection + 1) % 4
        | _ ->      currentDirection

    let getNewAntPosition (curPosition:int[]) direction =
        match direction with
        //north
        | 0 ->  let newY = curPosition.[1] - 1
                [| curPosition.[0]; ((newY) + ySize) % ySize;|]
        //east
        | 1 ->  let newX = curPosition.[0] + 1
                [|(newX % xSize); curPosition.[1];|]
        //south
        | 2 ->  let newY = curPosition.[1] + 1
                [| curPosition.[0]; (newY % ySize);|]     
        // west
        | 3 ->  let newX = curPosition.[0] - 1
                [| (newX + xSize) % xSize; curPosition.[1];|]
        | _ -> curPosition

    let rec mainLoop patternSize printAtInterval terminateAtInterval=

        let rec runAnt (directionArray:char[]) (antPos:int[]) antDir (board:int[,]) printCountdown terminateCountdown =
            let newDirection = turnAnt antDir (directionArray.[board.[antPos.[0], antPos.[1]]])
            let newPosition = getNewAntPosition antPos newDirection
            board.SetValue ((board.[antPos.[0], antPos.[1]] + 1) % (directionArray.Length), antPos)
        
//            match printCountdown with
//            | 1L -> printfn "began printing board at %s" (DateTime.Now.ToString())
//                    printBoardImage board
//                    printfn "new board generated at %s" (DateTime.Now.ToString())
//                    //Thread.Sleep(1500)
//                    if terminateCountdown < 1 then
//                        printfn "finished pattern %A" directionArray
//                        Thread.Sleep(5000)
//                    else
//                        runAnt directionArray newPosition newDirection board printAtInterval (terminateCountdown - 1)
//            | _ -> runAnt directionArray newPosition newDirection board (printCountdown - 1L) terminateCountdown 

            if printCountdown < 1L then
                (printBoardImage board) |> Async.StartAsTask |> ignore
                //printfn "new board generated at %s" (DateTime.Now.ToString())
                //Thread.Sleep(1500)
                if terminateCountdown < 1 then
                    printfn "finished pattern %A \n" directionArray
                else
                    runAnt directionArray newPosition newDirection board printAtInterval (terminateCountdown - 1)
            else
                runAnt directionArray newPosition newDirection board (printCountdown - 1L) terminateCountdown          

        let inputDirectionArray = Array.init patternSize (fun _ -> rand.Next(0, 2) |> generateDirection)
//        let inputDirectionArray = Array.init patternSize (fun index -> match index % 2 with
//                                                                       | 0  -> 'L'
//                                                                       | 1 -> 'R'
//                                                                       | _ -> 'E' )
        let board = Array2D.init xSize ySize (fun _ _ -> 0)
        runAnt inputDirectionArray initialPosition 0 board printAtInterval terminateAtInterval
        mainLoop patternSize printAtInterval terminateAtInterval

    // printMazeImage board
    mainLoop inputSize 5000000000L (10)
    printf "\nPress any key to continue..."
    Console.ReadKey(true) |> ignore
    0 // return an integer exit code
