﻿module problem003

open primes
open Xunit
open FsUnit.Xunit

//The prime factors of 13195 are 5, 7, 13 and 29.
//
//What is the largest prime factor of the number 600851475143 ?

let isXFactorOf x y =
  y % x = 0L

let findFirstFactor =
    function
    | x when x < 2L -> x
    | x ->  seq{2L..x}
            |> Seq.filter (fun (i:int64) -> isXFactorOf i x)
            |> Seq.head

let problem3Solver num =
  let action (state, more) = 
    match (state |> findFirstFactor), more with 
    | _, false -> None
    | fstFactor, _ when fstFactor >= state -> Some(fstFactor, (0L, false))
    | fstFactor, _ -> Some(fstFactor, ((state / fstFactor), true))
  
  Seq.unfold action (num , true)
  |> Seq.last

[<ProjectEuler.Problem(3, "Largest prime factor")>]
let problem3() =
  600851475143L |> problem3Solver

[<Fact>]
let ``13195 should equal 29``() =
  problem3Solver 13195L |> should equal 29L

[<Fact>]
let ``10 is a factor of 50``() =
  isXFactorOf 10L 50L |> should equal true

[<Fact>]
let ``11 is not a factor of 50``() =
  isXFactorOf 11L 50L |> should equal false

[<Fact>]
let ``first factor of 10 is 2 ``()=
  findFirstFactor 10L |> should equal 2L

[<Fact>]
let ``first factor of 25 is 5 ``()=
  findFirstFactor 25L |> should equal 5L

[<Fact>]
let ``answer``() = 
  let answer = problem3()
  answer |> should equal 6857L
  printfn "%i" answer
