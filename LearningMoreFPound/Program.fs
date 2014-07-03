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

    let size = ( Int32.Parse(Console.ReadLine()))

    let rec buildArray remainingRows intermediateList =
        if remainingRows > 0 then
            let newRow =  List.ofArray (Console.ReadLine().Split([|' '|]))
            let tempList = [newRow;]
            let newIntermediate = List.concat[intermediateList; tempList;]
            buildArray (remainingRows - 1) newIntermediate
        else
            intermediateList

    let testArray = buildArray size List.empty

    let rec transpose matrix = 
      match matrix with   // matrix is a list<list<int>>
      | row::rows ->      // case when the list of rows is non-empty
        match row with    // rows is a list<int>
        | col::cols ->    // case when the row is non-empty
          // Take first elements from all rows of the matrix
          let first = List.map List.head matrix
          // Take remaining elements from all rows of the matrix
          // and then transpose the resulting matrix
          let rest = transpose (List.map List.tail matrix) 
          first :: rest
        | _ -> []
      | _ -> [] 

    let rec printMatrix matrix =
        match matrix with
        | row::rows ->
            printfn "%A" row
            printMatrix matrix.Tail
        | _ -> ignore

    printMatrix testArray
    let test2 = transpose testArray


    printMatrix test2
    //printfn "%A" test2

    printf "\nPress any key to continue..."
    Console.ReadKey(true) |> ignore
    0 // return an integer exit code
