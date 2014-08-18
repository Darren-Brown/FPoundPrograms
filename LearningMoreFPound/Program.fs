﻿// Learn more about F# at http://fsharp.net
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
    let rand = new Random()
    
    let getInputFromFile (filepath:string)  =
        use sr = new StreamReader (filepath)
        let rec buildInputList (tempList:List<char>) (sr:StreamReader) =
            if not sr.EndOfStream then
                let temp = sr.ReadLine().ToCharArray() |> Array.toList
                buildInputList (List.append tempList temp) sr
            else
                tempList

        buildInputList List.Empty sr

    let input = (getInputFromFile "input.txt") 

    let rec learn (key:string) (inputList:List<char>) (dict:Map<string, List<char>>) keyLength =
        let curChar = inputList.Head
        if key.Length < keyLength then
            learn (key + curChar.ToString()) inputList.Tail dict keyLength
        else
            let newDict = match dict.ContainsKey key with
                            | true ->   let entry = dict.Item(key)
                                        let tempDict = dict.Remove(key)
                                        let newPossibilitesList = curChar :: entry
                                        tempDict.Add(key, newPossibilitesList)
                            | false ->  dict.Add(key, curChar::List.Empty)
            if inputList.Tail.IsEmpty then
                newDict
            else
                learn (key.Substring(1, key.Length - 1) + curChar.ToString()) inputList.Tail newDict keyLength
             
    let test = learn "" input Map.empty 3

    let rec generateString (dict:Map<string, List<char>>) (rnd:Random) =      

        let rec getKeyWithLeadingCapital (dict:Map<string, List<char>>) (rnd:Random) =
            let randomIndex = rnd.Next(1, dict.Count)
            let randomDictEntry = (Map.toArray dict).[randomIndex]
            let randomDictKey = fst randomDictEntry

            if Char.IsUpper(randomDictKey, 0) && not (randomDictKey.Contains(".") )then
                randomDictKey
            else
                getKeyWithLeadingCapital dict rnd

        let rec buildString (subString:List<char>) (key:string) (dict:Map<string, List<char>>) (rnd:Random) = 
            let curCharList = dict.Item key
            let randomCharIndex = rnd.Next(0, (curCharList.Length))
            let randomChar = curCharList.[randomCharIndex]

            if randomChar = '.' then
                (List.rev (randomChar :: subString)) |> List.toArray |> (fun s -> System.String s)
            else
                let newSubString = randomChar :: subString
                let newKey = (key.Substring(1, key.Length - 1) + randomChar.ToString())
                buildString newSubString newKey dict rnd

        let randomDictKey = getKeyWithLeadingCapital dict rnd
        buildString (randomDictKey.ToCharArray() |> Array.toList |> List.rev) randomDictKey dict rnd

//    for i = 0 to 10 do
//        let testString = generateString List.Empty test rand
//        printfn "%s" testString

    let rec infiniGen () =
        Console.Clear()
        printfn "%s" (generateString test rand)
        let input = Console.ReadKey(true).KeyChar
        if not (input.Equals 'x') then            
            infiniGen ()

    infiniGen()
    printf "\nPress any key to continue..."
    Console.ReadKey(true) |> ignore
    0 // return an integer exit code
