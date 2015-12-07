module problem016

open NUnit.Framework
open FsUnit

//Power digit sum
//Problem 16
//2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
//
//What is the sum of the digits of the number 2^1000?

let pow e v = bigint.Pow(v,e)

let charToNum c = System.Int32.Parse (string c)

let strToChars s = s.ToString().ToCharArray()

let numberToDigits = strToChars >> Seq.map charToNum

let solver (v:int32) e =
  bigint v |> pow e
  |> numberToDigits
  |> Seq.sum

[<ProjectEuler.Problem(16)>]
let problem16() = solver 2 1000
  
[<Test>]
let ``2 to pow 15 digit sum is 26``()=
  solver 2 15 |> should equal 26

[<Test>]
let ``ans``()=
  let ans = problem16()
  printfn "%A" ans
  ans |> should equal 1366
