module problem067

open problem018
open Xunit
open FsUnit.Xunit
open resources

let problem67() = 
  loadResourceAsText "p067_triangle.txt" |> solver

[<Fact>]
let ans() = 
  let ans = problem67()
  printfn "%A" ans
  ans |> should equal 7273