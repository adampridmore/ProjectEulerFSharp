module primesTests

open NUnit.Framework
open FsUnit
open primes

[<Test>]
let ``10 is not prime``() =
    isPrime 10 |> should equal false

[<Test>]
let ``0 is not prime``() =
    isPrime 0 |> should equal false

[<Test>]
let ``1 is not prime``() =
    isPrime 1 |> should equal false

[<Test>]
let ``2 is prime``() =
    isPrime 2 |> should equal true

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