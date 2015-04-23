module problem014

open NUnit.Framework
open FsUnit
open memoize

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
  
let collatzLengthCache = new System.Collections.Generic.Dictionary<_,_>() 

let collatzLength (n:int64) = 
  let rec collatzLengthRec n = 
    match collatzLengthCache.TryGetValue(n) with
    | (true, v) -> v
    | _ ->  let temp =  match n with
                        | n when n <= 0L -> 0L
                        | 1L -> 1L
                        | n -> (nextNum n |> collatzLengthRec) + 1L
            collatzLengthCache.Add(n, temp)
            temp

  collatzLengthRec n

let solver upTo =
  Seq.initInfinite (fun i -> int64 i+1L)
  |> Seq.takeWhile (fun i-> i < upTo)
  |> Seq.map (fun i -> i, collatzLength i)
  |> Seq.maxBy (fun (i, len) -> len)
  

let problem14 = 
  let ans, _ = solver 1000000L
  printfn "Answer = %i" ans
  ans |> should equal 837799L
  ans

[<Test>]
let ans()=
  problem14 |> should equal 837799L

[<Test>]
let ``Solution for up to 100``() = 
  let ans, _ = solver 100L
  printfn "Answer = %i" ans
  ans |> should equal 97L

[<Test>]
let ``nextNum for 13 is 40``()=
  nextNum 13L |> should equal 40L

[<Test>]
let ``nextNum for 40 is 20``()=
  nextNum 40L |> should equal 20L

[<Test>]
let ``collatzLength for 1 is 1``()=
  collatzLength 1L |> should equal 1L

[<Test>]
let ``collatzLength for 0 is 0``()=
  collatzLength 0L |> should equal 0L

[<Test>]
let ``collatzLength for 3 is 8``()=
  collatzLength 3L |> should equal 8L
    
// Below not used in solution anymore
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

let collatzSeq2 (n:int64) =
  let rec fn (n:int64) rest = 
    match n with
    | 1L -> [1L]
    | n ->  n :: fn (nextNum n) rest

  fn n []
  
[<Test>]
let ``collatzSeq for 13``()=
  let s = collatzSeq 13L
  s |> Seq.iter (printfn "%i")
  s |> should equal [|13L;40L;20L;10L;5L;16L;8L;4L;2L;1L|]

[<Test>]
let ``collatzSeq2 for 13``()=
  let s = collatzSeq2 13L
  s |> Seq.iter (printfn "%i")
  s |> should equal [|13L;40L;20L;10L;5L;16L;8L;4L;2L;1L|]

[<Test>]
let ``collatzSeq for large``()=
  collatzSeq 1000000L |> Seq.iter (printfn "%O")

[<Test>]
let ``collatzSeq2 for large``()=
  collatzSeq 1000000L |> Seq.iter (printfn "%O")

[<Test>]
let ``collatzSeq for 0``()=
  let s = collatzSeq 0L
  s |> Seq.iter (printfn "%i")
  s |> should equal [||]

[<Test>]
let scratch()=
  collatzSeq2 100L 
  |> Seq.iter (printfn "%O")

  collatzSeq 100L 
  |> Seq.iter (printfn "%O")