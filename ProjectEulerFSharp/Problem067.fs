module problem067

open problem018
open Xunit
open FsUnit.Xunit
open resources

[<ProjectEuler.Problem(67,"Maximum path sum II")>]
let problem67() = 
  loadResourceAsText "ProjectEulerFSharp.p067_triangle.txt" |> solver

[<Fact>]
let ans() = 
  let ans = problem67()
  printfn "%A" ans
  ans |> should equal 7273
