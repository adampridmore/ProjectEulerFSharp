﻿#r @"..\packages\FSPowerPack.Parallel.Seq.Community.3.0.0.0\Lib\Net40\FSharp.PowerPack.Parallel.Seq.dll"

open Microsoft.FSharp.Collections

let intToString (x:int) = x.ToString()
let charToString c = c.ToString()
let stringToInt (x:string) = System.Int32.Parse(x)
let toCharArray (x:string) = x.ToCharArray()
    
let toDigits = 
    intToString 
    >> toCharArray 
    >> Seq.map charToString 
    >> Seq.map stringToInt

let rec factorial x =
    match x with
    | 0-> 1
    | 1-> 1
    | x -> x * ( (x - 1) |> factorial )

let isCurious x =
    let ans = 
        x 
        |> toDigits
        |> Seq.map factorial
        |> Seq.sum
    x = ans

seq{3..100000}
|> PSeq.filter isCurious
|> PSeq.sum
|> (printfn "%d")

 
