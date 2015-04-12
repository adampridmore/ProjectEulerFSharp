module primes

open NUnit.Framework
open FsUnit

let isPrime numToCheck =
    seq{2L..numToCheck-1L} 
        |> Seq.exists (fun i -> numToCheck % i = 0L)
        |> not

let primes =
  let a = ResizeArray[2]
  let grow() =
    let p0 = a.[a.Count-1]+1
    let b = Array.create p0 true
    for di in a do
      let rec loop i =
        if i<b.Length then
          b.[i] <- false
          loop(i+di)
      let i0 = p0/di*di
      loop(if i0<p0 then i0+di-p0 else i0-p0)
    for i=0 to b.Length-1 do
      if b.[i] then a.Add(p0+i)
  fun n ->
    while n >= a.Count do
      grow()
    a.[n]

let primesSequence = 
  Seq.initInfinite (fun i -> i)
    |> Seq.map (fun i -> primes i)

[<Test>]
let ``10 is not prime``() =
    isPrime 10L |> should equal false

[<Test>]
let ``11 is prime``() =
    isPrime 11L |> should equal true
  
[<Test>]
let ``primes sequence``() =
  let ans = primesSequence |> Seq.take 5 ;
  ans |> Seq.iter (fun i -> printfn "%i" i)
  ans |> should equal [|2;3;5;7;11|]
