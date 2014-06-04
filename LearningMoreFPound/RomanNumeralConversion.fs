open System
open System.Text
open System.Threading
open System.Text.RegularExpressions
open System.Diagnostics
open Microsoft.FSharp.Reflection

let romanMapping = [(1000, 'M'); (500, 'D'); (100,'C'); (50,'L'); (10,'X'); (5,'V'); (1,'I');]
    printfn "Enter a number:"
    let (input:int) = int32( Console.ReadLine())
        
    let rec printRomans total (mapping:list<int * char>) = 
        if total > 0 then
            let currentValue = mapping.Head
            let romanValue = fst currentValue
            let romanChar = snd currentValue
            let remainder = total % romanValue
            let timesToPrint = (total - remainder) / romanValue

            for i = 1 to timesToPrint do
                printf "%c" romanChar

            //dis be wrong
            //needs to treat thousands, hundreds, tens, and ones as seperate values
            //ie 99 = 90 + 9, not 100 - 1

            //can only do subs with 1's, 10's, 100's, etc. not 5s
            if not mapping.Tail.IsEmpty then
                let nextValue = mapping.Tail.Head
                //special no 5 substitution rule
                let nextValue = 
                    if (romanValue / (fst mapping.Tail.Head)) = 2 then
                        mapping.Tail.Tail.Head
                    else
                        mapping.Tail.Head
                
                if romanValue <= (fst nextValue) + remainder then
                    let finalRemainder = remainder - (romanValue - fst nextValue)
                    printf "%c%c" (snd nextValue) romanChar
                    if not mapping.Tail.IsEmpty then
                        printRomans finalRemainder mapping.Tail
                else
            
                        printRomans remainder mapping.Tail


    printRomans input romanMapping
    printfn ""