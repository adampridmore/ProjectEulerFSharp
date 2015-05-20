module problem020

open NUnit.Framework
open FsUnit
open math
open textUtils

//  n! means n × (n − 1) × ... × 3 × 2 × 1
//
//  For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
//  and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.
//
//  Find the sum of the digits in the number 100!

let problem20 = 
  factorial (bigint 100)
  |> toString
  |> toCharArray
  |> Seq.map (fun c -> new string([|c|]))
  |> Seq.map (fun n -> bigint.Parse(n))
  |> Seq.sum   
  

[<Test>]
let ans() = 
  let ans = problem20
  printfn "%A" ans
  ans |> should equal 658