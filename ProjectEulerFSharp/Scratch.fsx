﻿#r @"..\packages\FSPowerPack.Parallel.Seq.Community.3.0.0.0\Lib\Net40\FSharp.PowerPack.Parallel.Seq.dll"
#load "primes.fs"

open Microsoft.FSharp.Collections

open primes

let toCharArray (s:string) = s.ToCharArray()

let rotate (list:List<'a>) = list.Tail @ [list.Head]

let getRotations lst =
    let rec getAll lst i = 
        match i with 
        | 0 -> [] 
        | _ -> lst :: (getAll (rotate lst) (i - 1))
    getAll lst (List.length lst)

let parseInt = System.Int32.Parse

let isCircularPrime n = 
    n
    |> string 
    |> toCharArray 
    |> Array.toList
    |> getRotations
    |> Seq.map (fun rot -> rot |> Seq.map string |> Seq.reduce(+) |> parseInt)
    |> Seq.exists  (fun x -> not (isPrime x))
    |> not

//let solver() = 
 
197 |> isCircularPrime 


seq{2..1000000-1}
|> PSeq.filter isCircularPrime
//|> Seq.map (fun x -> (printfn "%d" x);x)
|> Seq.length

