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
    let rand = new Random()
    
    printf "Please enter learning text: "
    let rec getInput (allInputs:List<string>) =
        let input = Console.ReadLine()
        if input = ";;" then
            allInputs
        else
            let inputList = input.Split([|' '|]) |> Array.toList
            getInput (List.append allInputs inputList)

    let input = getInput List.Empty

    let rec learn prevWord (inputList:List<string>) (dict:Map<string, List<string>>) =
        let curWord = inputList.Head
        let isStop = curWord.EndsWith(".")
        let cleanWord = match isStop with
                        | true -> curWord.Substring(0, curWord.Length - 1)
                        | false -> curWord
        let curKey = prevWord + " " + cleanWord

        let nextValue = match inputList.Tail.IsEmpty with
                        | true -> ""
                        | false ->  if inputList.Tail.Head.EndsWith(".") then
                                        inputList.Tail.Head.Substring(0, inputList.Tail.Head.Length - 1)
                                    else
                                        inputList.Tail.Head

        let newDict = match dict.ContainsKey curKey with
                        | true ->   let entry = dict.Item(curKey)
                                    let tempDict = dict.Remove(curKey)
                                    let newPossibilitesList = match isStop with 
                                                                | true -> "." :: (nextValue :: entry)
                                                                | false -> nextValue :: entry
                                    tempDict.Add(curKey, newPossibilitesList)
                        | false ->  match isStop with
                                    | true -> dict.Add(curKey, "." :: (nextValue::List.Empty))
                                    | false -> dict.Add(curKey, nextValue::List.Empty)
        if inputList.Tail.IsEmpty then
            newDict
        else
            learn cleanWord inputList.Tail newDict
             
    let test = learn input.Head input.Tail Map.empty

    let generateString (dict:Map<string, List<string>>) (rnd:Random) =
        let randomStart = rnd.Next(1, dict.Count)
        let randomStartList = (Map.toArray dict).[randomStart]
        let randomStartKey = (fst randomStartList)

        let rec printString currentKey (dict:Map<string, List<string>>) (rnd:Random) =
            let potentials = dict.Item(currentKey)
            let randomIndex = rnd.Next(0, potentials.Length)
            let randomString = potentials.[randomIndex]
            let newKey = (currentKey.Split([|' '|]).[1]) + " " + randomString
                    
            if newKey.Contains(".") then
                printf "\b%s" randomString
            else
                printf "%s " (randomString)
                printString newKey dict rnd

        printString randomStartKey dict rnd


    generateString test rand
    printf "\nPress any key to continue..."
    Console.ReadKey(true) |> ignore
    0 // return an integer exit code
