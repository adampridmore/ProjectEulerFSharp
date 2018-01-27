//Multiples of 3 and 5
//
//If we list all the natural numbers below 10 that are multiples 
//of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
//
//Find the sum of all the multiples of 3 or 5 below 1000.

let (|Divisible|_|) x = if x % 3 = 0 || x % 5 = 0 then Some() else None 
let (|InRange|_|) x = if x < 1000 then Some() else None

let rec fn x max =
    match x with
    | InRange -> 
        let newTotal = 
            match x with
            | Divisible -> x
            | _ -> 0
        newTotal + fn (x+1) max
    | _ -> 0

fn 1 1000
//233168