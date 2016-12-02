module problem026

open NUnit.Framework
open FsUnit

//A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with denominators 2 to 10 are given:
//
//1/2	= 	0.5
//1/3	= 	0.(3)
//1/4	= 	0.25
//1/5	= 	0.2
//1/6	= 	0.1(6)
//1/7	= 	0.(142857)
//1/8	= 	0.125
//1/9	= 	0.(1)
//1/10	= 	0.1
//Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit recurring cycle.
//
//Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.

[<ProjectEuler.Problem(26)>]
let problem26() = ProjectEuler.NotSolved // () //None //26

//let ( ** ) (a: bignum) (b) = BigNum.PowN (a,b)
//let ( ** ) (a: int) (b) = math.pow a b
let ( ** ) (a: bigint) (b: bigint) = bigint.Pow(a,b |> int)

[<Test>]
[<Ignore("Slow")>]
let scratch()=
//  seq{1..999} 
//  |> Seq.map (fun x -> 1N / bignum.FromInt x)
//  |> Seq.iter (fun x -> (printfn "%A") x)

  let repeatCycleLength (n:bigint) = 
    ((bigint 10 ** (n-bigint.One)) - bigint.One) / n
 
 // http://en.wikipedia.org/wiki/Repeating_decimal#Other_reciprocals_of_primes

  let res = 
    seq{for n in 1..1000 do yield bigint n}
    //|> Seq.map (fun x -> bigint x)
    |> Seq.map (fun x -> (x, repeatCycleLength x) )
    |> Seq.iter (fun (i,x) -> printfn "%A - %A" i x)

  

//  seq{for n in 1..1000 do yield bigint n}
//  //|> Seq.map (fun x -> bigint x)
//  |> Seq.mapi (fun i x -> (i, repeatCycleLength x) )
//  |> Seq.iter (fun (i,x) -> printfn "%i - %A" i x)
  ()

