module problem037

open FsUnit
open NUnit.Framework

open Microsoft.FSharp.Collections
open primes

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

let solver() = 
    Seq.initInfinite id
    |> Seq.where (fun v -> v > 10)
    |> Seq.filter isTruncatablePrime
    |> Seq.take 11
    |> Seq.sum

[<ProjectEuler.Problem(37)>]
let problem037() = solver()

[<Test>]
let solverTest() = 
    problem037() |> should equal 748317

