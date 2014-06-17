    printfn "enter number of digits to count to"
    let digits = int32 (Console.ReadLine().ToString())
                   
    let rec recFizzBuzz currentDigit finalDigit =
        match currentDigit with
        | i when i % 3 = 0 && i % 5 = 0 -> printf "FizzBuzz"
        | i when i % 3 = 0 -> printf "Fizz"
        | i when i % 5 = 0 -> printf "Buzz"
        | _ -> printf "%d" currentDigit

        if (currentDigit < finalDigit) then
            printf ", "
            recFizzBuzz (currentDigit + 1) finalDigit


    let rec Fibb (first:UInt64) (second:UInt64) remaining=
//        if (first + second) > (uint64 -1)then
            match remaining with
            | 1 -> printfn "%d." (first + second)
            | _ ->
                  if (first + second) > (uint64 0) then 
                      printf "%d, " (first + second)
                      Fibb second (first + second) (remaining - 1)
                  else
                      printf "1, "
                      Fibb second (uint64 1) (remaining - 1)
//        else
//            printfn "\nError: value exceeds maximum value for Int64 types."


    recFizzBuzz 1 digits
    printfn "\n"
    let test = (uint64 0)
    Fibb test test digits