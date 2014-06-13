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

    let wumpusPercent = 0.15
    let pitTrapPercent = 0.05
    let goldPercent = 0.15
    let weaponPercent = 0.15

    printf "Enter dungeon size: "
    let dungeonSize = int32 (Console.ReadLine())

    let buildDungeon size = 
        let generator = new Random(DateTime.Now.Millisecond)
        let emptyDungeon = Array2D.create (size + 2) (size + 2) '.'

        let wallDungeon size (dungeon:char[,]) =
            let adjustedSize = size
            for i = 0 to adjustedSize do
                dungeon.SetValue( '#', [|0; i;|])
                dungeon.SetValue( '#', [|adjustedSize; i;|])
                dungeon.SetValue( '#', [|i; 0;|])
                dungeon.SetValue( '#', [|i; adjustedSize;|])
            dungeon

        let walledDungeon = wallDungeon (size + 1) emptyDungeon

        let entrancePosition = [|generator.Next(1, size); generator.Next(1, size);|]
        walledDungeon.SetValue('^', entrancePosition)

        let addElements symbol percent dungeonSize (dungeon:char[,]) =
            let elementNumbers = int32 (Math.Floor(percent * float (size * size)))
            let rec generatePositions numToGenerate dungeonSize (positions:List<(int * int)>) (dungeon:char[,]) =
                if numToGenerate > 0 then
                    let dimX, dimY = generator.Next(1, dungeonSize), generator.Next(1, dungeonSize)
                    if not (List.exists (fun x -> (dimX, dimY) = x) positions) && (dungeon.[dimX, dimY] = '.') then
                        let newPosition = (dimX, dimY)
                        if newPosition = (entrancePosition.[0], entrancePosition.[1]) then
                            printfn "walla"
                        let newList = List.Cons(newPosition, positions)
                        generatePositions (numToGenerate - 1) dungeonSize newList dungeon
                    else
                        generatePositions numToGenerate dungeonSize positions dungeon
                else
                    positions

            let elementPositions = generatePositions elementNumbers dungeonSize List.Empty dungeon
            for item in elementPositions do
                dungeon.SetValue(symbol, [|fst item; snd item|])

            
            printfn "wakka"

        let wumpusNumbers = Math.Floor(wumpusPercent * float (size * size))
        addElements '!' wumpusPercent (size + 1) walledDungeon
        addElements 'W' weaponPercent (size + 1) walledDungeon
        addElements 'P' pitTrapPercent (size + 1) walledDungeon
        addElements '$' goldPercent (size + 1) walledDungeon

//        let pitTrapNumbers = 

        printfn "%A" walledDungeon
        printfn "wakka"
    
    buildDungeon dungeonSize

    printf "\nPress any key to continue..."
    Console.ReadKey(true) |> ignore
    0 // return an integer exit code
