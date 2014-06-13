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

    printf "Enter dungeon size: "
    let dungeonSize = int32 (Console.ReadLine())

    let buildDungeon size = 
        let emptyDungeon = Array2D.create (size + 1) (size + 1) '.'

        let wallDungeon size (dungeon:char[,]) =
            let adjustedSize = size - 1
            for i = 0 to adjustedSize do
                dungeon.SetValue( '#', [|0; i;|])
                dungeon.SetValue( '#', [|adjustedSize; i;|])
                dungeon.SetValue( '#', [|i; 0;|])
                dungeon.SetValue( '#', [|i; adjustedSize;|])
            dungeon

        let walledDungeon = wallDungeon (size + 1) emptyDungeon
        printfn "%A" walledDungeon
        printfn "wakka"
    
    buildDungeon dungeonSize

    printf "\nPress any key to continue..."
    Console.ReadKey(true) |> ignore
    0 // return an integer exit code
