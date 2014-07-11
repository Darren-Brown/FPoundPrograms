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

    let getNotAceCardValue cardString =
        match cardString with
        | "Two" -> 2
        | "Three" -> 3
        | "Four" -> 4
        | "Five" -> 5
        | "Six" -> 6
        | "Seven" -> 7
        | "Eight" -> 8
        | "Nine" -> 9
        | "Ten" | "Jack" | "Queen" | "King" -> 10
        | _ -> 0

    let handleAces subTotal aces =
        if aces > 0 then
            if subTotal + 11 <= 21 then
                subTotal + 11 + (aces - 1)
            else
                subTotal + aces
        else
            subTotal

    let rec getHandValue (cardsInHand:List<string>) subTotal aces cardCount=
        if not cardsInHand.IsEmpty then
            let currentCard = cardsInHand.Head
            match currentCard with
            | "Ace" -> getHandValue cardsInHand.Tail subTotal (aces + 1) (cardCount + 1)
            | _ ->  match (getNotAceCardValue currentCard) with
                    | x when x > 0 -> getHandValue cardsInHand.Tail (subTotal + x) aces (cardCount + 1)
                    | x when x <= 0 -> getHandValue cardsInHand.Tail (subTotal + x) aces (cardCount)
                    | _ -> getHandValue cardsInHand.Tail subTotal aces (cardCount)
        else
            let finalTotal = handleAces subTotal aces
            if (cardCount > 4) && (finalTotal <= 21) then
                22
            else
                if finalTotal > 21 then
                    0
                else
                    finalTotal

    let rec HandleInput winnersString winningTotal =
        let inputString = Console.ReadLine()
        if not (inputString.Equals String.Empty) then
            let nameHand = inputString.Split([|':'|])
            if nameHand.Length > 1 then
                let handValue = getHandValue (List.ofArray(nameHand.[1].Split([|' '|]))) 0 0 0
                match handValue with
                | x when x > winningTotal -> HandleInput nameHand.[0] handValue
                | x when x = winningTotal -> HandleInput "Tie" handValue
                | _ -> HandleInput winnersString winningTotal
            else
                HandleInput winnersString winningTotal          
        else
            if winnersString = "Tie" || winnersString = String.Empty then
                printf "Tie"
            else
                if winningTotal = 22 then
                    printf "%s has won with a five card trick" winnersString
                else
                    printf "%s has won with a hand of %d" winnersString winningTotal
        
    HandleInput "No one" 0

    printf "\nPress any key to continue..."
    Console.ReadKey(true) |> ignore
    0 // return an integer exit code
