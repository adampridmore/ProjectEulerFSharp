#r @"..\packages\FSPowerPack.Parallel.Seq.Community.3.0.0.0\Lib\Net40\FSharp.PowerPack.Parallel.Seq.dll"
#load "primes.fs"
#load "Seq.fs"

open primes
open FSharp.Collections.ParallelSeq

let charArrayToString (ca: char array) = 
    new string(ca |> Seq.toArray)

let arrayBackTail = Array.tail 
let arrayFrontTail = Array.rev >> Array.tail >> Array.rev

let tryStrip stripper (x: int) = 
    match x.ToString().ToCharArray() with
    | [||] -> None
    | [|v|] -> None
    | v ->  v    
            |> stripper
            |> charArrayToString 
            |> System.Int32.Parse
            |> Some

let tryStripLeft (x: int) =  x |> tryStrip arrayBackTail
let tryStripRight (x: int) =  x |> tryStrip arrayFrontTail

let stripSequence stripper number = 
    number 
    |> Some 
    |> Seq.unfold (fun i -> 
        match i with
        | None -> None
        | Some(v) -> Some(v, v |> stripper) ) 
        
let stripLeftSequence = stripSequence tryStripLeft 
let stripRightSequence = stripSequence tryStripRight

let isTrue x = x = true

let isTruncatablePrime x =
    (x
    |> stripLeftSequence 
    |> Seq.map isPrime 
    |> Seq.forall isTrue)
    &&
    (x
    |> stripRightSequence
    |> Seq.map isPrime 
    |> Seq.forall isTrue)

Seq.initInfinite id
|> Seq.where (fun v -> v > 10)
//|> Seq.map(fun x -> printfn "%d" x; x)
//|> PSeq.filter isTruncatablePrime
|> Seq.filter isTruncatablePrime
//|> Seq.take 11
|> Seq.takeWhile (fun x -> x < 1000000)
//|> PSeq.sum
|> Seq.sum

//748317

