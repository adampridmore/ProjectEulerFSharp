module problem030

open FsUnit.Xunit
open Xunit
open FSharp.Collections.ParallelSeq

let charToNumber (c:char) = System.Convert.ToInt32(c.ToString()) 
let pow b e = 
    System.Math.Pow(b |> float, e|> float) |> int

let numberToDigits number = 
    number.ToString().ToCharArray()
    |> Seq.map charToNumber

let isSumPowerOfDigits number exponent =
    let sumPowerOfDigits =
        numberToDigits number
        |> Seq.map (fun n -> pow n exponent)
        |> Seq.sum
        
    sumPowerOfDigits = number

[<ProjectEuler.Problem(30,"Digit fifth powers")>]
let problem30() = 
    seq{10..999999}
    |> PSeq.filter(fun x -> isSumPowerOfDigits x 5)
    |> Seq.sum

[<Fact>]
let ans() = 
    problem30() |> should equal 443839





