module problem002

// Even Fibonacci numbers
//
// Each new term in the Fibonacci sequence is generated 
// by adding the previous two terms. By starting 
// with 1 and 2, the first 10 terms will be:
//
// 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
//
// By considering the terms in the Fibonacci sequence whose values do not 
// exceed four million, find 
// the sum of the even-valued terms.

open Xunit
open FsUnit.Xunit
open Fibonacci

let evenFibSum maxValue = 
  let isEven i = i % 2 = 0
  
  fibseq 
  |> Seq.takeWhile (fun i -> i<=maxValue)
  |> Seq.filter isEven
  |> Seq.sum

[<ProjectEuler.Problem(2)>]
let problem2() = 
  evenFibSum 4000000

[<Fact>]
let ``test``() =
  evenFibSum 89 |> should equal 44
   
[<Fact>]
let ``answer``() = 
  let answer = problem2()
  answer |> should equal 4613732
  printfn "%i" answer  
    