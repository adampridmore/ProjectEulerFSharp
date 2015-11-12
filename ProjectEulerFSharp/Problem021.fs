module problem021

open NUnit.Framework
open FsUnit
open math
open primes

//  Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
//  If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.
//
//  For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.
//
//  Evaluate the sum of all the amicable numbers under 10000.

let isAmicable a =
  let b = properDivisorsSum a
  a = properDivisorsSum b && a<>b

let solver n =
  seq{1..n-1}
  |> Seq.filter isAmicable
  |> Seq.sum

let problem21() =
  solver 10000

[<Test>]
let ``220 is amicable number``()=
  isAmicable 220 |> should equal true
    
[<Test>]
let ``221 is not amicable number``()=
  isAmicable 221 |> should equal false

[<Test>]
let ``496 is not amicable number as proper divisors sum is itself``()=
  isAmicable 221 |> should equal false
      
[<Test>]
let ans() = 
  let ans = problem21()
  printfn "%A" ans
  ans |> should equal 31626

