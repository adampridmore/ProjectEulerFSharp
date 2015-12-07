module problem005

open NUnit.Framework
open FsUnit

open primes

// 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

// What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

let isXDivisibleByNumsUpTo (x:bigint) upTo =
  seq{1..upTo}
  |> Seq.exists (fun i -> x % bigint i <> bigint.Zero)
  |> not

let getProductOfPrimesUpTo x : bigint = 
  primesSequence 
  |> Seq.takeWhile  (fun i -> i < x )
  |> Seq.map bigint
  |> Seq.reduce (*)

let smallestNumberDivisibleUpTo upTo =
  let increment = upTo |> getProductOfPrimesUpTo 

  Seq.initInfinite (fun i -> (bigint i + bigint.One) * increment)
  |> Seq.find(fun i -> isXDivisibleByNumsUpTo i upTo)

[<ProjectEuler.Problem(5)>]
let problem5() = smallestNumberDivisibleUpTo 20 

[<Test>]
let ``smallest number divisible up to 10``() =
  smallestNumberDivisibleUpTo 10 |> should equal (bigint 2520)

[<Test>]
let ``answer``() =
  let ans = problem5()
  printfn "%A" ans
  ans |> should equal (bigint 232792560)

[<Test>]
let ``is x divisible by nums for 100``()=
  isXDivisibleByNumsUpTo (bigint 100) 10 |> should equal false

[<Test>]
let ``is x divisible by nums for 2520``()=
  isXDivisibleByNumsUpTo (bigint 2520) 10 |> should equal true