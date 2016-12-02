﻿#r @"..\packages\FSPowerPack.Parallel.Seq.Community.3.0.0.0\Lib\Net40\FSharp.PowerPack.Parallel.Seq.dll"
#load "primes.fs"
#load "Seq.fs"

open Microsoft.FSharp.Collections

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

585 |> isBinaryAndDecimalPalendrome
586 |> isBinaryAndDecimalPalendrome

seq{0..1000000-1}
|> Seq.filter isBinaryAndDecimalPalendrome
//|> Seq.iter (printfn "%d")
|> Seq.sum


//872187