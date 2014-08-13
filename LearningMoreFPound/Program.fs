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

type probabilityContainer =
    {
    Name:string;
    Count:int;
    }

[<EntryPoint>]
let main argv = 
    let rand = new Random()
    
    let getInputFromFile (filepath:string)  =
        use sr = new StreamReader (filepath)
        let rec buildInputList (tempList:List<string>) (sr:StreamReader) =
            if not sr.EndOfStream then
                let temp = sr.ReadLine().Split([|' '|]) |> Array.toList
                buildInputList (List.append tempList temp) sr
            else
                tempList

        buildInputList List.Empty sr

    let input = (getInputFromFile "input.txt") |> Seq.toList 

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
                                                                | true -> "." :: entry
                                                                | false -> nextValue :: entry
                                    tempDict.Add(curKey, newPossibilitesList)
                        | false ->  match isStop with
                                    | true -> dict.Add(curKey, "." :: List.Empty)
                                    | false -> dict.Add(curKey, nextValue::List.Empty)
        if (inputList.Tail.IsEmpty) || inputList.Tail.Tail.IsEmpty then
            newDict
        else
            if isStop then
                learn nextValue inputList.Tail.Tail newDict
            else
                learn cleanWord inputList.Tail newDict
             
    let test = learn input.Head input.Tail Map.empty

    let rec buildActionList (dict:List<string * List<string>>) (rnd:Random) (partialList:List<probabilityContainer>) =
        if dict.IsEmpty then
            partialList
        else
            let entry = dict.Head
            let key = fst entry
            if key.Contains("I ") then
                let entryCount = snd entry
                buildActionList dict.Tail rnd ({ Name=key; Count=entryCount.Length;} :: partialList)
            else
                buildActionList dict.Tail rnd partialList

    let generateAction (dict:Map<string, List<string>>) (rnd:Random) =
        

        let rec getActionKey (dict:Map<string, List<string>>) (rnd:Random) =
            let rec getProbTotal (list:List<probabilityContainer>) subtotal =
                if list.IsEmpty then
                    subtotal
                else
                    getProbTotal list.Tail (subtotal + list.Head.Count)
            
            let rec getActionFromInt subtotal (list:List<probabilityContainer>) actionValue  =
                if list.IsEmpty then
                    "Action not found"
                else
                    if (subtotal + list.Head.Count) > actionValue then
                        list.Head.Name
                    else
                        getActionFromInt (subtotal + list.Head.Count) (list.Tail)  actionValue
            
            let actions = buildActionList (Map.toList(dict)) rnd list.Empty
            let randomActionInt = rnd.Next(0, (getProbTotal actions 0))
            getActionFromInt 0 actions randomActionInt


        let rec printString currentKey (dict:Map<string, List<string>>) (rnd:Random) =
            if dict.ContainsKey(currentKey) then
                let potentials = dict.Item(currentKey)
                let randomIndex = rnd.Next(0, potentials.Length)
                let randomString = potentials.[randomIndex]
                let newKey = (currentKey.Split([|' '|]).[1]) + " " + randomString
                    
                if newKey.Contains(".") then
                    printfn "\b%s" randomString
                else
                    printf "%s " (randomString)
                    printString newKey dict rnd
            else
                printfn "\b."

        let startKey = getActionKey dict rnd
        printf "%s " startKey
        printString startKey dict rnd


    for i = 0 to 10 do
        generateAction test rand
    printf "\nPress any key to continue..."
    Console.ReadKey(true) |> ignore
    0 // return an integer exit code
