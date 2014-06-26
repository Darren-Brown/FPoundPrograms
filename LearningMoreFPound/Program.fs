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
    let fizzy n =
        match n with
        | x when ((x % 3) = 0 && (x % 5 = 0)) -> Some("FizBuz", n + 1)
        | x when ((x % 3) = 0) -> Some("Fizz", n + 1)
        | x when ((x % 5 = 0)) -> Some("Buzz", n + 1)
        | x when x > 100 -> None
        | _ -> Some ( n.ToString(), n + 1)

    let doubleIt n =
        n |> Seq.unfold(fun x -> fizzy x)        

    doubleIt -100 |> Seq.iter (fun x -> printf "%s \t" x)


    printf "\nPress any key to continue..."
    Console.ReadKey(true) |> ignore
    0 // return an integer exit code
