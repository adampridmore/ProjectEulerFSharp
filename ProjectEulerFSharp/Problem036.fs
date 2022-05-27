module problem036

open FsUnit.Xunit
open Xunit

open FSharp.Collections.ParallelSeq

let intToBinaryString (number: int) = 
    System.Convert.ToString(number, 2)

let intToDecimaltring (number: int) = 
    System.Convert.ToString(number, 10)

let isPalendromeString (s:string) = 
    let ca = s.ToCharArray()
    ca = ( ca |> Array.rev)

let isBinaryAndDecimalPalendrome i = 
    let isBinaryPalendrome = intToBinaryString >> isPalendromeString 
    let isDecimalPalendrome = intToDecimaltring >> isPalendromeString 

    (i |> isBinaryPalendrome) && (i |> isDecimalPalendrome)

let solver() = 
    seq{0..1000000-1}
    |> Seq.filter isBinaryAndDecimalPalendrome
    |> Seq.sum
    //872187

[<ProjectEuler.Problem(36,"Double-base palindromes")>]
let problem036() = solver()

[<Fact>]
let solverTest() = 
    problem036() |> should equal 872187

