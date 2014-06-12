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

    let rec SumThirdFib firstNum secondNum curTotal =
        let evenNum = firstNum + secondNum
        if (firstNum < 4000000) && (secondNum < 4000000) && (evenNum < 4000000) then
            let newFirst = secondNum + evenNum
            let newSecond = evenNum + newFirst
            SumThirdFib newFirst newSecond (evenNum + curTotal)
        else 
            curTotal

    printfn "the sum is %d" (SumThirdFib 3 5 2)
    

    printf "\nPress any key to continue..."
    Console.ReadKey(true) |> ignore
    0 // return an integer exit code
