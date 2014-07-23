// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.


open System
open System.Collections
open System.Text
open System.Threading
open System.Text.RegularExpressions
open System.Diagnostics
open Microsoft.FSharp.Reflection

type Suit = Club | Diamond | Heart | Spade 
type Rank = Two | Three | Four | Five | Six | Seven | Eight 
            | Nine | Ten | Jack | Queen | King | Ace

[<EntryPoint>]
let main argv = 

    let compareCard card1 card2 = 
        if card1 < card2 
        then printfn "%A is greater than %A" card2 card1 
        else printfn "%A is greater than %A" card1 card2     

    let hand = [ Ace,Club; King,Heart; Ace,Spade; Ace,Heart; Two,Diamond;]

    let deck = [for cardType in [Club; Diamond; Heart; Spade;] do
                    for cardRank in [Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King; Ace;] do
                        yield (cardRank, cardType)
                ]

    let scramble (sqn : seq<'T>) = 
        let rnd = new Random()
        let rec scramble2 (sqn : seq<'T>) = 
            /// Removes an element from a sequence.
            let remove n sqn = sqn |> Seq.filter (fun x -> x <> n)
 
            seq {
                let x = sqn |> Seq.nth (rnd.Next(0, sqn |> Seq.length)) // get a random element out of the sequence
                yield x //return the random element
                let sqn' = remove x sqn //remove the random element from the sequence
                if not (sqn' |> Seq.isEmpty) then
                    yield! scramble2 sqn' // recurse to get another random element from the newly shrunken list
            }
        scramble2 sqn

    let printCards =
        Seq.iter (printf "%A ")

    let printHand hand=
        printf "Hand: "
        printCards hand
        printfn ""

    let shuffls = scramble deck

    //printfn "%A" (Seq.toList shuffls)
    let temp = fst deck.Head
    let tempHand = Seq.take 5 shuffls
    
    let test = Seq.skip 5 deck
    let tempHand2 = Seq.take 5 test
    //printf "Hand:"
    printHand tempHand
    //printfn "Hand2: %A" tempHand2
    //printfn "%A" temp
    //instant sorting!
    //List.sort hand |> printfn "sorted hand is (low to high) %A"

    printf "\nPress any key to continue..."
    Console.ReadKey(true) |> ignore
    0 // return an integer exit code
