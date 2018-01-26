module problem034

open FsUnit.Xunit
open Xunit

open FSharp.Collections.ParallelSeq

let intToString (x:int) = x.ToString()
let charToString c = c.ToString()
let stringToInt (x:string) = System.Int32.Parse(x)
let toCharArray (x:string) = x.ToCharArray()
    
let toDigits = 
    intToString 
    >> toCharArray 
    >> Seq.map charToString 
    >> Seq.map stringToInt

let rec factorial (x:int) =
    math.factorial (bigint x)

let isCurious x =
    let ans = 
        x 
        |> toDigits
        |> Seq.map factorial
        |> Seq.sum
    (bigint x) = ans

[<ProjectEuler.Problem(34)>]
let problem034() = 
    seq{3..100000}
    |> PSeq.filter isCurious
    |> PSeq.sum
    
[<Fact>]
let ans()=
    problem034() |> should equal 40730
 
