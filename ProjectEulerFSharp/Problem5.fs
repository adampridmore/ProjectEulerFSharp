module Problem5

open primes
open NUnit.Framework
open FsUnit

// 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

// What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

let isXDivisibleByNumsUpTo x upTo =
  seq{1..upTo}
  |> Seq.exists (fun i -> x % i <> 0)
  |> not

let getProductOfPrimesUpTo x = 
  let product = (primesSequence 
    |> Seq.takeWhile (fun i -> i < x )
    |> Seq.reduce (fun acc ele -> acc * ele))
  int product

let smallestNumberDivisibleUpTo upTo =
  let increment = getProductOfPrimesUpTo upTo

  Seq.initInfinite (fun i -> (i+1) * increment)
  |> Seq.find(fun i -> isXDivisibleByNumsUpTo i upTo)

let problem5 = 
  smallestNumberDivisibleUpTo 20 

[<Test>]
let ``smallest number divisible up to 10``() =
  smallestNumberDivisibleUpTo 10 |> should equal 2520

[<Test>]
let ``answer``() =
  let ans = problem5
  printfn "%i" ans
  ans |> should equal 232792560

[<Test>]
let ``is x divisible by nums for 100``()=
  isXDivisibleByNumsUpTo 100 10 |> should equal false

[<Test>]
let ``is x divisible by nums for 2520``()=
  isXDivisibleByNumsUpTo 2520 10 |> should equal true
