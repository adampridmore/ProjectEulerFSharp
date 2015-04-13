module Problem5

open NUnit.Framework
open FsUnit

open primes

// 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

// What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

let isXDivisibleByNumsUpTo (x:int64) upTo =
  seq{1..upTo}
  |> Seq.exists (fun i -> x % int64 i <> 0L)
  |> not

let getProductOfPrimesUpTo x : int64 = 
  let product = (primesSequence 
    |> Seq.takeWhile (fun i -> i < x )
    |> Seq.map int64
    |> Seq.reduce (fun (acc:int64) (ele:int64) -> acc * ele))
  product

let smallestNumberDivisibleUpTo upTo =
  let increment = getProductOfPrimesUpTo upTo

  Seq.initInfinite (fun i -> ((int64 i)+1L) * increment)
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
  isXDivisibleByNumsUpTo 100L 10 |> should equal false

[<Test>]
let ``is x divisible by nums for 2520``()=
  isXDivisibleByNumsUpTo 2520L 10 |> should equal true

//[<Test>]
//let ``scratch``() =
//  seq{20..25} 
//  |> Seq.map (fun i -> (i, smallestNumberDivisibleUpTo i)) 
//  |> Seq.iter (fun (i, ans) -> printfn "%i -> %i" i ans)
  