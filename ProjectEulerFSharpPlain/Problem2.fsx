// Even Fibonacci numbers
//
// Each new term in the Fibonacci sequence is generated 
// by adding the previous two terms. By starting 
// with 1 and 2, the first 10 terms will be:
//
// 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
//
// By considering the terms in the Fibonacci sequence whose values do not 
// exceed four million, find 
// the sum of the even-valued terms.

let (|Even|Odd|) input = if input % 2L = 0L then Even else Odd
let (|InRange|NotInRange|) x = if x < 4000000L then InRange else NotInRange

//let withinRange x = x < 4000000L
let rec fib (a,b) total =
    match a with
    | InRange -> 
        let newTotal = 
            match a with
            | Even -> a + total
            | Odd  -> total
        fib (b, a+b) (newTotal)
    | NotInRange -> total
    
fib (1L,2L) 0L
// 4613732L

// 1+ 2+ 3+ 5+ 8+ 13+ 21+ 34+ 55+ 89
// 2+ 8+ 34