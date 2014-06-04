open System
open System.Text
open System.Threading
open System.Text.RegularExpressions
open System.Diagnostics
open Microsoft.FSharp.Reflection


let memorizationTime = 5000

let generator = new System.Random()

let pattern = generator.Next(1, 1000).ToString ()

let rec gameLoop (pattern:string) =
        
    printfn "The pattern is:"
    printfn ""
    printfn "%s" pattern
    Thread.Sleep(memorizationTime)

    Console.Clear ()

    printfn "Please enter the pattern now:"

    let input = Console.ReadLine ()
    if input = pattern then
        printfn "youre winner"
        printfn ""
        gameLoop (pattern + generator.Next(1, 10).ToString())
    else
        printfn "youre loser"
        printfn "pattern was %s" pattern


gameLoop pattern