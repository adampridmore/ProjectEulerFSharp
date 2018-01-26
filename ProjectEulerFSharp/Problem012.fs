﻿module problem012

open Xunit
open FsUnit.Xunit
open primes

//Highly divisible triangular number
//Problem 12
//The sequence of triangle numbers is generated by adding the natural numbers. So the 7th triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28. The first ten terms would be:
//
//1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
//
//Let us list the factors of the first seven triangle numbers:
//
// 1: 1
// 3: 1,3
// 6: 1,2,3,6
//10: 1,2,5,10
//15: 1,3,5,15
//21: 1,3,7,21
//28: 1,2,4,7,14,28
//We can see that 28 is the first triangle number to have over five divisors.
//
//What is the value of the first triangle number to have over five hundred divisors?

let triangleNumbers = 
  Seq.unfoldInf (fun (cur, i) -> cur, (cur+i, i+1) ) (1,2)

let getFactorsCount i =
  let factors = primeFactors i
  match factors with 
  | [] -> 0
  | _ ->  factors 
          |> Seq.groupBy id
          |> Seq.map (fun (_, nums) -> nums |> Seq.length)
          |> Seq.map (fun i -> i + 1)
          |> Seq.reduce (*)

let solver numberOfDivisors =
  triangleNumbers 
  |> Seq.find (fun i -> getFactorsCount i >= numberOfDivisors)

[<ProjectEuler.Problem(12)>]
let problem12() = 
  solver 500
  
[<Fact>]
let ``answer``() =
  let ans = problem12()
  printfn "%i" ans
  ans |> should equal 76576500

[<Fact>]
let ``first 4 triangle numbers``()=
  let ans = triangleNumbers |> Seq.take 4 |> Seq.toList
  ans |> printfn "%A"
  ans |> should equal [1;3;6;10]

[<Fact>]
let ``first triangle number to have over five divisors is 28``()=
  solver 5 |> should equal 28

[<Fact>]
let ``Number of factors of 10 is 4``()=
  getFactorsCount 10 |> should equal 4

[<Fact>]
let ``Number of factors of 0 is 0``()=
  getFactorsCount 0 |> should equal 0

[<Fact>]
let ``Number of factors of 1 is 1``()=
  getFactorsCount 1 |> should equal 0

[<Fact>]
let ``Number of factors of 30 is 8``()=
  getFactorsCount 30 |> should equal 8

[<Fact>]
let ``scratch``() = 
 triangleNumbers |> Seq.take 100|> Seq.iter (printfn "%i")
  

// Not used in solution
let triangleNumber i = 
  triangleNumbers 
  |> Seq.take i 
  |> Seq.last
    
let getFactors i =
  Seq.init i (fun j -> i - j)
  |> Seq.filter (fun j -> i % j = 0)

[<Fact>]
let ``3rd triangle number is``()=
  triangleNumber 3 |> should equal 6