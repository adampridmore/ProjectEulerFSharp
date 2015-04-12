module Problem9

open primes
open NUnit.Framework
open FsUnit

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

let isPythagoreanTriplet a b c =
  if a>b || b>c then false
  else sqr a + sqr b = sqr c

let problem9 = 
  let (a,b,c) = (seq { 
      for a in 1..1000 do
        for b in a..1000 do 
            yield (a,b, 1000 - a - b)
      }
    |> Seq.find (fun (a,b,c) -> isPythagoreanTriplet a b c))
    
  a*b*c

[<Test>]
let ``scratch``() =
  printfn "%i" problem9
   
[<Test>]
let ``answer``() =
  let ans = problem9
  printfn "%i" ans
  ans |> should equal 31875000
    
[<Test>]
let ``a=3 b=4 c=5 is a pythagorean triplet ``()=
  isPythagoreanTriplet 3 4 5 |> should equal true

[<Test>]
let ``a=3 b=4 c=6 is not a pythagorean triplet ``()=
  isPythagoreanTriplet 3 4 6 |> should equal false


[<Test>]
let ``a=5 b=4 c=3 is not a pythagorean triplet ``()=
  isPythagoreanTriplet 5 4 3 |> should equal false