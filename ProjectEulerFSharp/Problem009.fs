module problem009

open primes
open Xunit
open FsUnit.Xunit

//Special Pythagorean triplet
//Problem 9
//A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
//
//a^2 + b^2 = c^2
//For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
//
//There exists exactly one Pythagorean triplet for which a + b + c = 1000.
//Find the product abc.

let sqr a = a*a

let isPythagoreanTriplet a b c= 
    match a,b,c with
    | a, b, c when a>b || b>c -> false
    | a, b, c -> sqr a + sqr b = sqr c

[<ProjectEuler.Problem(9,"Special Pythagorean triplet")>]
let problem9() = 
    let sequence = 
        seq {
            for a in 1..1000 do
                for b in a + 1..1000 do 
                    yield (a,b, 1000 - a - b)
        }

    let (a,b,c) = sequence |> Seq.find (fun (a,b,c) -> isPythagoreanTriplet a b c)
    
    a*b*c

[<Fact>]
let ``scratch``() =
  printfn "%i" (problem9())
   
[<Fact>]
let ``answer``() =
  let ans = problem9()
  printfn "%i" ans
  ans |> should equal 31875000
    
[<Fact>]
let ``a=3 b=4 c=5 is a pythagorean triplet ``()=
  isPythagoreanTriplet 3 4 5 |> should equal true

[<Fact>]
let ``a=3 b=4 c=6 is not a pythagorean triplet ``()=
  isPythagoreanTriplet 3 4 6 |> should equal false


[<Fact>]
let ``a=5 b=4 c=3 is not a pythagorean triplet ``()=
  isPythagoreanTriplet 5 4 3 |> should equal false
