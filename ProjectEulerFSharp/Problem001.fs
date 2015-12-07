module problem001

open ProjectEuler
open NUnit.Framework
open FsUnit

//Multiples of 3 and 5
//
//If we list all the natural numbers below 10 that are multiples 
//of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
//
//Find the sum of all the multiples of 3 or 5 below 1000.
let solver upperBound = 
  let isDivisibleBy5or3 i = i % 3 = 0 || i % 5 = 0

  seq{1..upperBound-1}
  |> Seq.filter isDivisibleBy5or3
  |> Seq.sum

[<Problem(1)>]
let problem1() = 
  // Find the sum of all the multiples of 3 or 5 below 1000.
  solver 1000
   
[<Test>]
let ``answer``() = 
  let answer = problem1()
  answer |> should equal 233168
  printfn "%i" answer


[<Test>]
let ``answer for up to 10 is 23``() = 
  let answer = solver 10
  answer |> should equal 23
  printfn "%i" answer