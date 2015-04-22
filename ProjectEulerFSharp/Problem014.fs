module problem014

open NUnit.Framework
open FsUnit

//Longest Collatz sequence
//
//The following iterative sequence is defined for the set of positive integers:
//
//  n → n/2 (n is even)
//  n → 3n + 1 (n is odd)
//
//Using the rule above and starting with 13, we generate the following sequence:
//
//13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
//It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.
//
//Which starting number, under one million, produces the longest chain?
//
//NOTE: Once the chain starts the terms are allowed to go above one million.

let nextNum n = 
  let isOdd x =
    x % 2L = 0L

  match n with 
  | 1L -> 1L
  | n when n |> isOdd -> n / 2L
  | n -> 3L * n + 1L

let collatzSeq (n:int64) = 
  match n with
  | 0L -> Seq.empty
  | _ -> 
    let nums = (
      Seq.unfold (fun state -> 
                    match state with
                    | 1L -> None
                    | state -> Some(state, nextNum state)
        ) n
      )

    Seq.append nums [|1L|]
   
let solver upTo =
  Seq.initInfinite (fun i -> int64 i+1L)
  |> Seq.takeWhile (fun i-> i < upTo)
  |> Seq.map (fun i -> i, collatzSeq i |> Seq.length)
  |> Seq.maxBy (fun (i, len) -> len)

let problem14 = 
  let ans, _ = solver 1000000L
  printfn "Answer = %i" ans
  ans |> should equal 837799L
  ans

[<Test>]
let scratch()=
  let i, len = solver 1000000L
  printfn "Answer = %i, len=%i" i len
  collatzSeq i |> Seq.iter (printfn "%i")
  
[<Test>]
let ``collatzSeq for 13``()=
  let s = collatzSeq 13L
  s |> Seq.iter (printfn "%i")
  s |> should equal [|13L;40L;20L;10L;5L;16L;8L;4L;2L;1L|]

[<Test>]
let ``collatzSeq for 0``()=
  let s = collatzSeq 0L
  s |> Seq.iter (printfn "%i")
  s |> should equal [||]
  
[<Test>]
let ``nextNum for 13 is 40``()=
  nextNum 13L |> should equal 40L

[<Test>]
let ``nextNum for 40 is 20``()=
  nextNum 40L |> should equal 20L