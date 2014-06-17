// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.


open System
open System.Text
open System.Threading
open System.Text.RegularExpressions
open System.Diagnostics
open Microsoft.FSharp.Reflection

[<EntryPoint>]
let main argv = 

    let printStacksWithSelection (stacks:List<int>[]) (selectedItem:int) cursorPosition=
        for i = 0 to 2 do
            printf "%A" stacks.[i]
            if i = cursorPosition then
                printf " * "
            if i = selectedItem then
                printf "<-"
            printfn ""    

    let rec generateStacks sizeOfStack (partialStack:List<int>)=
        match sizeOfStack with
        | 0 ->  [|partialStack; List<int>.Empty; List<int>.Empty;|]
        | _ ->  generateStacks (sizeOfStack - 1) (List.Cons(sizeOfStack, partialStack))

    let moveDisc sourceStack targetStack (stacks:List<int>[]) =
        if (not stacks.[sourceStack].IsEmpty) && ((stacks.[targetStack].IsEmpty) || (stacks.[targetStack].Head > stacks.[sourceStack].Head)) then
            let newTargetStack = List.Cons(stacks.[sourceStack].Head, stacks.[targetStack])
            let newSourceStack = stacks.[sourceStack].Tail
            let missingStack =  3 - sourceStack - targetStack
            match (sourceStack + targetStack) with
            | 1 ->  if sourceStack > targetStack then
                        [| newTargetStack; newSourceStack; stacks.[missingStack];|]
                    else
                        [| newSourceStack; newTargetStack; stacks.[missingStack];|]
            | 2 ->  if sourceStack > targetStack then
                        [| newTargetStack; stacks.[missingStack]; newSourceStack;|]
                    else
                        [| newSourceStack; stacks.[missingStack]; newTargetStack;|]
            | _ -> if sourceStack > targetStack then
                        [| stacks.[missingStack]; newTargetStack; newSourceStack;|]
                    else
                        [| stacks.[missingStack]; newSourceStack; newTargetStack;|]
        else
            stacks

    let checkForGoalState (stacks:List<int>[]) =
        if (stacks.[0].IsEmpty && stacks.[1].IsEmpty) then
            true
        else
            false
    let initialStacks = generateStacks 1 List<int>.Empty

    let rec gameLoop (stacks:List<int>[]) cursorPosition selectionPosition =
        Console.Clear()
        printStacksWithSelection stacks cursorPosition selectionPosition
        let input = Console.ReadKey().Key.ToString()
        match input with
        | "UpArrow" ->      gameLoop stacks ((cursorPosition + 2)% 3) selectionPosition
        | "DownArrow" ->    gameLoop stacks ((cursorPosition + 1)% 3) selectionPosition
        | "Spacebar" ->     if selectionPosition > -1 then
                                let newStacks = moveDisc selectionPosition cursorPosition stacks
                                if (not (checkForGoalState newStacks)) then
                                    gameLoop newStacks cursorPosition -1
                                else
                                    Console.Clear()
                                    printStacksWithSelection newStacks cursorPosition -1
                                    printfn "You win"
                            else
                                gameLoop stacks cursorPosition cursorPosition
        | "Escape" ->       printfn "Goodbye"
        | _ ->              gameLoop stacks cursorPosition selectionPosition

    gameLoop initialStacks 0 -1
    printf "\nPress any key to continue..."
    Console.ReadKey(true) |> ignore
    0 // return an integer exit code
