module Problem6

open primes
open NUnit.Framework
open FsUnit

//The sum of the squares of the first ten natural numbers is,
//
//1^2 + 2^2 + ... + 10^2 = 385
//The square of the sum of the first ten natural numbers is,
//
//(1 + 2 + ... + 10)^2 = 552 = 3025
//Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 − 385 = 2640.
//
//Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
//

let sqr i = i * i

let solver maxNumber = 
  let numbers = seq{1..maxNumber}
 
  let squareSum = (numbers
    |> Seq.map sqr
    |> Seq.sum)
  
  let sum = numbers |> Seq.sum
  (sqr sum) - squareSum

let problem6 = 
  solver 100

[<Test>]
let ``answer``() =
  let ans = problem6
  printfn "%i" ans
  ans |> should equal 25164150

[<Test>]
let ``solver for 10 is 2640``()=
  solver 10 |> should equal 2640
