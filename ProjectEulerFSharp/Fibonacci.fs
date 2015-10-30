module Fibonacci

open NUnit.Framework
open FsUnit

// It seems to be undefined if the first two terms are 0,1 or 1,1 which 
// can lead to off by one errors. In problem 2 it assume 1,1
let fibseqgeneral firstTwoTerms add = 
  let action (a,b) = Some( add a b, (b, add a b) )
  let restOfSequence = Seq.unfold action firstTwoTerms
    
  Seq.append  [|fst firstTwoTerms; snd firstTwoTerms|] restOfSequence

let fibseq = 
  fibseqgeneral (1,1) (fun a b -> a + b)

let fibseqbig = 
  fibseqgeneral (bigint 1, bigint 1) (fun a b -> a + b)

[<Test>]
let small_set() = 
    fibseq |> Seq.take 5 |> Array.ofSeq |> should equal [1;1;2;3;5]

[<Test>]
let specific_large_item() = 
    fibseq |> Seq.nth 20 |> should equal 10946     