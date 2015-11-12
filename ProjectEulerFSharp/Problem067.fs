module problem067

open problem018
open NUnit.Framework
open FsUnit
open resources

let problem67() = 
  loadResourceAsText "p067_triangle.txt" |> solver

[<Test>]
let ans() = 
  let ans = problem67()
  printfn "%A" ans
  ans |> should equal 7273