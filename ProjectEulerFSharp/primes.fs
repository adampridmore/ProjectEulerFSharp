module primes

open NUnit.Framework
open FsUnit

let limit = 1000000
//returns an array of all the primes up to limit
let table =
    let table = Array.create limit true //use bools in the table to save on memory
    let tlimit = int (sqrt (float limit)) //max test no for table, ints should be fine
    let mutable curfactor = 1;
    while curfactor < tlimit-2 do
        curfactor <- curfactor+2
        if table.[curfactor]  then //simple optimisation
            let mutable v = curfactor*2
            while v < limit do
                table.[v] <- false
                v <- v + curfactor
    let out = Array.create (100000) 0 //this needs to be greater than pi(limit)
    let mutable idx = 1
    out.[0]<-2
    let mutable curx=1
    while curx < limit-2 do
        curx <- curx + 2
        if table.[curx] then
            out.[idx]<-curx
            idx <- idx+1
    out

let isPrime numToCheck =
    seq{2L..numToCheck-1L} 
        |> Seq.exists (fun i -> numToCheck % i = 0L)
        |> not


[<Test>]
let ``10 is not prime``() =
    isPrime 10L |> should equal false

[<Test>]
let ``11 is prime``() =
    isPrime 11L |> should equal true

let primesSequence = 
  Seq.initInfinite (fun i -> int64(i+1))
  |> Seq.filter (fun i -> isPrime i)

[<Test>]
let ``primes sequence``() =
  primesSequence |> Seq.take 5 |> should equal [|1L;2L;3L;5L;7L|]