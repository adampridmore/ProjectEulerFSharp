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
    x % 2 = 0

  match n with 
  | 1 -> 1
  | n when n |> isOdd -> n / 2
  | n -> 3 * n + 1

let collatzSeq n = 
  match n with
  | 0 -> Seq.empty
  | _ -> 
    let nums = (
      Seq.unfold (fun state -> 
                    match state with
                    | 1 -> None
                    | state -> Some(state, nextNum state)
        ) n
      )

    Seq.append nums [|1|]
    
let problem14 = 
  14

let solver upTo =
  Seq.initInfinite (fun i -> i+1)
  |> Seq.takeWhile (fun i-> i < upTo)
  |> Seq.map (fun i -> i, collatzSeq i |> Seq.length)
  |> Seq.maxBy (fun (i, len) -> len)

[<Test>]
let scratch()=
  let i, len = solver 1000
  printfn "Answer = %i, len=%i" i len
  collatzSeq i |> Seq.iter (printfn "%i")
  
[<Test>]
let ``collatzSeq for 13``()=
  let s = collatzSeq 13 
  s |> Seq.iter (printfn "%i")
  s |> should equal [|13;40;20;10;5;16;8;4;2;1|]

[<Test>]
let ``collatzSeq for 0``()=
  let s = collatzSeq 0 
  s |> Seq.iter (printfn "%i")
  s |> should equal [||]
  
[<Test>]
let ``nextNum for 13 is 40``()=
  nextNum 13 |> should equal 40

[<Test>]
let ``nextNum for 40 is 20``()=
  nextNum 40 |> should equal 20