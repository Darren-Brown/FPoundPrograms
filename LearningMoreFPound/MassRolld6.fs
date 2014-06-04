open System
open System.Text
open System.Threading
open System.Text.RegularExpressions
open System.Diagnostics
open Microsoft.FSharp.Reflection




let rec massRollD6 totalRolls (resultsMap:Map<int, int>) (numberGenerator:Random) =
    if totalRolls > 0 then 
        let result = numberGenerator.Next(1,3)
        let remainingRolls = totalRolls - 1

        if resultsMap.ContainsKey(result) then
            let currentResultCount = resultsMap.Item(result)
            let resultsMinusCurrentCase = resultsMap.Remove(result)
            let newResults = resultsMinusCurrentCase.Add(result, currentResultCount + 1)
            massRollD6 remainingRolls newResults numberGenerator
        else
            let newResults = resultsMap.Add(result, 1)
            massRollD6 remainingRolls newResults numberGenerator
    else
        resultsMap

let printResults totalRolls (results:Map<int, int>) =
    printfn "Rolls: %d" totalRolls
    for entry in results do
        let (percentage:float) = (float entry.Value) / (float totalRolls)
        printfn "%d : %f%c" entry.Key percentage '%'

let die = new System.Random()

for i = 1 to 10 do
    let results = massRollD6 (pown 10 i) Map.empty die
    printResults (pown 10 i) results