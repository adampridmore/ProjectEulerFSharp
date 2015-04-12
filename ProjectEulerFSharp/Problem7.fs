module Problem7

open primes
open NUnit.Framework
open FsUnit

//10001st prime
//Problem 7
//By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
//
//What is the 10,001st prime number?

let getNthPrime n =
  primesSequence |> Seq.skip n |> Seq.head

let problem7 = 
  getNthPrime 10001

[<Test>]
let ``answer``() =
  let ans = problem7
  printfn "%i" ans
  ans |> should equal 104743

[<Test>]
let ``6th prime is 13``() =
  getNthPrime 6 |> should equal 13

[<Test>]
let ``scratch``()=
  //  let ans = primesSequence |> Seq.skip 10001 |> Seq.head
  //  printfn "%i" ans
  ()