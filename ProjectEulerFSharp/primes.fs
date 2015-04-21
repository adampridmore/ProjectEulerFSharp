module primes

open NUnit.Framework
open FsUnit

let isPrime n = 
    let upperBound = int (sqrt (float n))
    let hasDivisor =     
        [2..upperBound]
        |> List.exists (fun i -> n % i = 0)
    not hasDivisor

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
  Seq.initInfinite (fun i -> i) |> Seq.map primes

let primeFactors i = 
  let rec fac n x a = 
    if x = n then
      x::a
    elif n % x = 0 then 
      fac (n/x) x (x::a)
    else
      fac n (x+1) a

  match i with
  | 0 -> []
  | 1 -> []
  | _ -> fac i 2 []

[<Test>]
let ``10 is not prime``() =
    isPrime 10 |> should equal false

[<Test>]
let ``11 is prime``() =
    isPrime 11 |> should equal true
  
[<Test>]
let ``primes sequence``() =
  let ans = primesSequence |> Seq.take 5 ;
  ans |> Seq.iter (printfn "%i")
  ans |> should equal [|2;3;5;7;11|]

[<Test>]
let ``prime factorisation of 0 is nothing``() =
  primeFactors 0 |> Seq.sort |> should equal [||]  

[<Test>]
let ``prime factorisation of 1 is nothing``() =
  primeFactors 1 |> Seq.sort |> should equal [||]  

[<Test>]
let ``prime factorisation of 2 is 2``() =
  primeFactors 2 |> Seq.sort |> should equal [|2|]  

[<Test>]
let ``prime factorisation of 3 is 3``() =
  primeFactors 3 |> Seq.sort |> should equal [|3|]  

[<Test>]
let ``prime factorisation of 4  2 & 2``() =
  primeFactors 4 |> Seq.sort |> should equal [|2;2|]  
  
[<Test>]
let ``prime factorisation of 10 are ...``() =
  let ans = primeFactors 10 |> Seq.sort
  ans |> Seq.iter (printfn "%i")
  ans |> should equal [|2;5|]

[<Test>]
let ``prime factorisation of 25 are ...``() =
  let ans = primeFactors 25 
  ans |> Seq.iter (printfn "%i")
  ans |> should equal [|5;5|]

[<Test>]
let ``prime factorisation of prime 17``() =
  let ans = primeFactors 17
  ans |> Seq.iter (printfn "%i")
  primeFactors 17 |> should equal [|17|]

[<Test>]
let ``get prime factors of first 5 numbers``()=
  seq{0..5} 
  |> Seq.map primeFactors
  |> Seq.iter (printfn "%A")