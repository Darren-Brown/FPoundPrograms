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

    let rec SumThreesAndFives currentSum currentNumber maxNumber =
        if currentNumber <= maxNumber then
            if (currentNumber % 5) = 0 || (currentNumber % 3) = 0 then
                SumThreesAndFives (currentSum + currentNumber) (currentNumber + 1) maxNumber
            else
                SumThreesAndFives currentSum (currentNumber + 1) maxNumber
        else
            currentSum

    printfn "the sum is %d" (SumThreesAndFives 0 1 1000)
    

    printf "\nPress any key to continue..."
    Console.ReadKey(true) |> ignore
    0 // return an integer exit code
