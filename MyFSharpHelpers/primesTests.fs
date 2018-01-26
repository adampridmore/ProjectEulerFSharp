module primesTests

open Xunit
open FsUnit.Xunit

open primes

[<Fact>]
let ``10 is not prime``() =
    isPrime 10 |> should equal false

[<Fact>]
let ``0 is not prime``() =
    isPrime 0 |> should equal false

[<Fact>]
let ``1 is not prime``() =
    isPrime 1 |> should equal false

[<Fact>]
let ``2 is prime``() =
    isPrime 2 |> should equal true

[<Fact>]
let ``11 is prime``() =
    isPrime 11 |> should equal true
  
[<Fact>]
let ``primes sequence``() =
  let ans = primesSequence |> Seq.take 5 |> Seq.toList
  ans |> Seq.iter (printfn "%i")
  ans |> should equal [2;3;5;7;11]

[<Fact>]
let ``prime factorisation of 0 is nothing``() =
  primeFactors 0 |> Seq.sort |> Seq.toList |> should equal List.empty<System.Int32>

[<Fact>]
let ``prime factorisation of 1 is nothing``() =
  primeFactors 1 |> Seq.sort |> Seq.toList |> should equal List.empty<System.Int32>

[<Fact>]
let ``prime factorisation of 2 is 2``() =
  primeFactors 2 |> Seq.sort |> Seq.toList |> should equal [2]

[<Fact>]
let ``prime factorisation of 3 is 3``() =
  primeFactors 3 |> Seq.sort |> Seq.toList |> should equal [3]  

[<Fact>]
let ``prime factorisation of 4  2 & 2``() =
  primeFactors 4 |> Seq.sort |> Seq.toList |> should equal [2;2]  
  
[<Fact>]
let ``prime factorisation of 10 are ...``() =
  let ans = primeFactors 10 |> Seq.sort |> Seq.toList 
  ans |> Seq.iter (printfn "%i")
  ans |> should equal [2;5]

[<Fact>]
let ``prime factorisation of 25 are ...``() =
  let ans = primeFactors 25 |> Seq.toList 
  ans |> Seq.iter (printfn "%i")
  ans |> should equal [5;5]

[<Fact>]
let ``prime factorisation of prime 17``() =
  let ans = primeFactors 17 |> Seq.toList 
  ans |> Seq.iter (printfn "%i")
  primeFactors 17 |> should equal [17]

[<Fact>]
let ``get prime factors of first 5 numbers``()=
  seq{0..5} 
  |> Seq.map primeFactors
  |> Seq.iter (printfn "%A")