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

    let colorIncrement = 255 / inputDirectionArray.Length
    let colorArray = Array.init (inputDirectionArray.Length) (fun index -> Color.FromArgb((index * colorIncrement), (Math.Max(0, (index * colorIncrement - 150))), (index * colorIncrement)))

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

    let printMaze (maze:int[,]) =
        use wr = new StreamWriter ("maze.txt", false)
        //let sizeString = (areaSize + 2).ToString() + " " + (ySize + 2).ToString()
        //wr.WriteLine (sizeString)
        for i = 0 to (areaSize - 1) do
            for j = 0 to (areaSize - 1) do
                wr.Write(maze.[i,j])
            wr.WriteLine()

    let printMazeImage (maze:int[,]) =
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
    

    let rec mainLoop (antPosition:int[]) antDirection (board:int[,]) count=

        let newDirection = turnAnt antDirection (inputDirectionArray.[board.[antPosition.[0], antPosition.[1]]])
        let newPosition = getNewAntPosition antPosition newDirection
        board.SetValue ((board.[antPosition.[0], antPosition.[1]] + 1) % (inputDirectionArray.Length), antPosition)
        
//        Console.Clear()
//        printBoard board
//        Thread.Sleep(50)
//        mainLoop newPosition newDirection board 0
        if count > 20000000 then
            //Console.Clear()
            printfn "began printing board at %s" (DateTime.Now.ToString())
            //printBoard board
            
            //printMaze board
            printMazeImage board
            printfn "new board generated at %s" (DateTime.Now.ToString())
            //Console.ReadKey(true) |> ignore
            mainLoop newPosition newDirection board 0
        else
            mainLoop newPosition newDirection board (count + 1)


    printMazeImage board
    mainLoop initialPosition 0 board 0
    printf "\nPress any key to continue..."
    Console.ReadKey(true) |> ignore
    0 // return an integer exit code
