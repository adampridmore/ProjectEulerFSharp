﻿module problem007

open primes
open Xunit
open FsUnit.Xunit

//10001st prime
//Problem 7
//By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
//
//What is the 10,001st prime number?

[<ProjectEuler.Problem(7,"10001st prime")>]
let problem7() = 
  // 10,001st prime is at 10,000 index
 primes 10000

[<Fact>]
let ``answer``() =
  let ans = problem7()
  printfn "%i" ans
  ans |> should equal 104743

[<Fact>]
let ``6th prime is 13``() =
  primes 5 |> should equal 13
