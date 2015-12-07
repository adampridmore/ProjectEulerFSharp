module problem010

open primes
open NUnit.Framework
open FsUnit

//Summation of primes
//
//Problem 10
//
//The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
//
//Find the sum of all the primes below two million.

let sumPrimesBelow maxPrime = 
  primesSequence 
  |> Seq.takeWhile (fun i -> i < maxPrime) 
  |> Seq.map int64
  |> Seq.sum

[<ProjectEuler.Problem(10)>]
let problem10() = sumPrimesBelow 2000000

[<Test>]
let ``sum of all primes below 10 is 17``() =
  sumPrimesBelow 10 |> should equal 17
   
[<Test>]
let ``answer``() =
  let ans = problem10()
  printfn "%i" ans
  ans |> should equal 142913828922L
