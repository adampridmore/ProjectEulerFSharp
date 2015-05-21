module math

open NUnit.Framework
open FsUnit
open primes

let pow x y = 
  let rec powInt x y acc =
    match y with
    | 0 -> 1 
    | 1 -> x * acc 
    | n -> let newAcc = x * acc
           powInt x (y-1) newAcc

  powInt x y 1

let rec factorial (x:bigint) = 
  let rec facInt (x:bigint) acc =
    let newAcc = acc * x
    match x with
    | x when x = bigint.One -> newAcc
    | x -> facInt (x - bigint.One) (newAcc)

  facInt x bigint.One

let properDivisors n = 
  [1..n-1]
  |> Seq.filter (fun i -> n % i = 0)

// Taken from 
// http://math.stackexchange.com/questions/22721/is-there-a-formula-to-calculate-the-sum-of-all-proper-divisors-of-a-number
let properDivisorsSum n = 
  match primeFactors n with
  | [] -> 0
  | x ->  ( x 
            |> Seq.countBy (fun x -> x) 
            |> Seq.map (fun (x,count) -> seq{for i in 0..count do yield pow x i} |> Seq.sum)
            |> Seq.reduce (*)   
          ) - n  

[<Test>]
let ``proper divisors sum of 120 is 240``()=
  120 |> properDivisorsSum |> should equal 240


[<Test>]
let ``get proper divisors of 10``()=
  10 |> properDivisors |> should equal [1;2;5]

[<Test>]
let ``pow: 2 to the power of 3 is 8``()=
  pow 2 3 |> should equal 8

[<Test>]
let ``pow: 1 to the power of 3 is 1``()=
  pow 1 3 |> should equal 1

[<Test>]
let ``pow: 0 to the power of anything is 0``()=
  pow 0 3 |> should equal 0

[<Test>]
let ``pow: anything to the power of 0 is 1``()=
  pow 3 0 |> should equal 1

[<Test>]
let ``pow: 1 to the power of a large is 1``()=
  pow 1 1000000 |> should equal 1
  
[<Test>]
let ``factorial 3 equals 6``()=
  factorial (bigint 3) |> should equal (bigint (1*2*3) )

[<Test>]
let ``factorial 10 equals 3,628,800``()=
  factorial (bigint  10) |> should equal (bigint 3628800 )