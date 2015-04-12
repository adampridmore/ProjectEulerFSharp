module Problem1
//Multiples of 3 and 5
//
//If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
//
//Find the sum of all the multiples of 3 or 5 below 1000.

open NUnit.Framework
open FsUnit

let isXMultipleOf x y = 
    x % y = 0    

let isMultipleOf3Or5 i = 
    isXMultipleOf i 3 || isXMultipleOf i 5

let solver upperBound = 
        seq{1..upperBound-1}
        |> Seq.filter isMultipleOf3Or5
        |> Seq.sum

let problem1 = 
    // Find the sum of all the multiples of 3 or 5 below 1000.
    solver 1000
   
[<Test>]
let ``answer``() = 
    let answer = problem1 
    answer |> should equal 233168
    printfn "%i" answer  
    
[<Test>]
let ``is 10 multiple of 5``() = 
    isXMultipleOf 10 5 |> should be True

[<Test>]
let ``is 10 multiple of 4``() = 
    isXMultipleOf 10 4 |> should be False
    