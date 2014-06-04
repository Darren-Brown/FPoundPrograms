open System
open System.Text
open System.Threading
open System.Text.RegularExpressions
open System.Diagnostics
open Microsoft.FSharp.Reflection

[<EntryPoint>]
let main argv = 

    // get input
    // parse inputs for different die rolls
    // for each dice roll
    // parse roll for die denomination, number of dice
    // for number of dice
    // generate a new roll
    // print roll + " "
    // add roll to total
    // roll next die
    // return total
    // roll next set of die
    // print total of all die sets

    printfn "Enter your die rolls (NdM + ...):"

    let input = Console.ReadLine()

    let noWSInput = input.Replace(" ", "")
    let flagNegs = input.Replace("-", "-@")

    let dieRolls = List.ofArray (flagNegs.Split([|'-'; '+'|]))

    let generator = new System.Random ()
    let rollAllDice (rolls:List<string>) (generator:Random)=
        let RolldM (denomination:int) (generator:Random) =
            let rollValue = generator.Next(1, (denomination + 1))
            printf "%d " rollValue
            rollValue
        
        let rec RollNdM (diceToRoll:int) (denomination:int) (generator:Random) (currentTotal:int) =
            match diceToRoll with
            | 0 -> currentTotal
            | _ -> RollNdM (diceToRoll - 1) denomination generator (currentTotal + (RolldM denomination generator))

        let rec sumRolls (rolls:List<string>) (generator:Random) total =
            if rolls.IsEmpty then
                total
            else
                let rollInfo = rolls.Head.Split([|'d'|])
                if rollInfo.Length = 1 then
                    let checkNegs = rollInfo.[0].Split([|'@'|])
                    if checkNegs.Length = 1 then
                        printfn "+ %d" (int32 checkNegs.[0])
                        sumRolls rolls.Tail generator (total + (int32 checkNegs.[0]))
                    else
                        printfn "- %d" (int32 checkNegs.[1])
                        sumRolls rolls.Tail generator (total - (int32 checkNegs.[1]))
                else
                    if rollInfo.[0] = "" then
                        let rollValue = RollNdM 1 (int32 rollInfo.[1]) generator 0
                        printfn "= %d" rollValue
                        sumRolls rolls.Tail generator (total + rollValue)
                    else
                        let rollValue = RollNdM (int32 rollInfo.[0]) (int32 rollInfo.[1]) generator 0
                        printfn "= %d" rollValue
                        sumRolls rolls.Tail generator (total + rollValue)
        
        sumRolls rolls generator 0   
    
    printfn "=%d" (rollAllDice dieRolls generator)