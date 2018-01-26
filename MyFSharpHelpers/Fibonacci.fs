module Fibonacci

open Xunit
open FsUnit.Xunit

//.Xunit It seems to be undefined if the first two terms are 0,1 or 1,1 which 
// can lead to off by one errors. In problem 2 it assume 1,1
let fibseqgeneral firstTwoTerms add = 
  Seq.unfoldInf (fun (a,b) -> a, (b, add a b) ) firstTwoTerms

let fibseq = fibseqgeneral (1,1) (+)
let fibseqbig = fibseqgeneral (bigint 1, bigint 1) (+)

[<Fact>]
let ``small set``() = 
    fibseq |> Seq.take 5 |> Seq.toList |> should equal [1;1;2;3;5]

[<Fact>]
let ``specific large item``() = 
    fibseq |> Seq.item 20 |> should equal 10946

