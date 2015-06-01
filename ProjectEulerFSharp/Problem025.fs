module problem025

open NUnit.Framework
open FsUnit
open math
open textUtils
open Fibonacci

let digitCount number = number |> string |> String.length

let solver numberOfDigits = 
  fibseqbig
  |> Seq.findIndex (fun n -> (digitCount n) = numberOfDigits  )

let problem25 = 1 + solver 1000

[<Test>]
let ans()=
  let ans = problem25
  printfn "%i" ans
  ans |> should equal 4782
  
[<Test>]
let scratch()=
  fibseqbig
  |> Seq.findIndex (fun n -> (digitCount n) = 3  )
  |> printfn "%A"
