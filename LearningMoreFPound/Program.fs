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
    
    let generateDirection inputVal=
        match inputVal with
        | 0 -> 'L'
        | 1 -> 'R'
        | _ -> 'E'         

    let minusXIterName = "iteration-{0}.bmp"
    let imageStorageNumber = 3

    // for background
//    let xSize = 100
//    let ySize = 100
//    let xSize = 1920
//    let ySize = 1080
    let xSize = 3840
    let ySize = 2160

    let redLimiter = 255
    let greenLimiter = 31
    let blueLimiter = 1

    let inputSize = Math.Max(Math.Max(redLimiter, greenLimiter), blueLimiter) * 255

    let colorIncrement = Math.Max(1, 255 / inputSize)
    let colorFunction index = Color.FromArgb((index / redLimiter) % 255, ( index / greenLimiter) % 255,  (index / blueLimiter) % 255)

    let initialPosition = [|(xSize/2); (ySize/2)|]

    let printBoardImage (maze:int[,]) = async{
        printfn "generating bitmap at %s" (DateTime.Now.ToString())
        let test = new Bitmap(xSize, ySize)
        for i = 0 to (xSize - 1) do
            for j = 0 to (ySize - 1) do
                test.SetPixel(i, j, (colorFunction maze.[i,j]))
        
        

        let rec shuffleImageNumbers iterNumber =
            if iterNumber > 0 then
                if File.Exists(String.Format(minusXIterName, iterNumber - 1)) then   
                    File.Delete(String.Format(minusXIterName, iterNumber))          
                    File.Move(String.Format(minusXIterName, iterNumber - 1), String.Format(minusXIterName, iterNumber))

                shuffleImageNumbers (iterNumber - 1)

        shuffleImageNumbers imageStorageNumber
        
        test.Save(String.Format(minusXIterName, 0), System.Drawing.Imaging.ImageFormat.Bmp) 
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

        let rec runAnt (directionArray:char[]) (antPos:int[]) antDir (board:int[,]) iterationsUntilPrintCountdown terminateCountdown (printTask:Tasks.Task<unit>)=
            let newDirection = turnAnt antDir (directionArray.[board.[antPos.[0], antPos.[1]]])
            let newPosition = getNewAntPosition antPos newDirection
            board.SetValue ((board.[antPos.[0], antPos.[1]] + 1) % (directionArray.Length), antPos)

            if iterationsUntilPrintCountdown < 1L then
                if printTask <> null then
                    printTask.Wait ()

                let newTask = (printBoardImage board) |> Async.StartAsTask
                if terminateCountdown < 1 then
                    printfn "finished pattern %A \n" directionArray
                else
                    runAnt directionArray newPosition newDirection board printAtInterval (terminateCountdown - 1) newTask
            else
                runAnt directionArray newPosition newDirection board (iterationsUntilPrintCountdown - 1L) terminateCountdown printTask          

        let inputDirectionArray = Array.init patternSize (fun _ -> rand.Next(0, 2) |> generateDirection)

        let board = Array2D.init xSize ySize (fun _ _ -> 0)
        runAnt inputDirectionArray initialPosition 0 board printAtInterval terminateAtInterval null
        mainLoop patternSize printAtInterval terminateAtInterval

    mainLoop inputSize 1000000000L (100)
    printf "\nPress any key to continue..."
    Console.ReadKey(true) |> ignore
    0 // return an integer exit code
