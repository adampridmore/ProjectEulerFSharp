module problem027

open primes
open FSharp.Collections.ParallelSeq
open FsUnit.Xunit
open Xunit

let sqr x = x * x

let f a b n = (n |> sqr) + (a * n) + b

let abs = 
    function
    | x when x >= 0 -> x
    | x -> -x

let tester a b = 
    Seq.initInfinite id
    |> Seq.map (f a b)
    |> Seq.map abs
    |> Seq.map (fun x -> isPrime x)
    |> Seq.takeWhile id
    |> Seq.length

[<ProjectEuler.Problem(27)>]
let problem27() = 
    let minMaxRange = 999

    seq{ for a in -minMaxRange..minMaxRange do
           for b in -minMaxRange..minMaxRange do
                yield (a,b)
    }
    |> PSeq.map (fun (a,b) -> (a,b,(tester a b) ) )
    |> Seq.maxBy(fun (_,_,length) -> length)
    //|> (fun (a,b,l) -> (a,b,l,(a * b)) )
    |> (fun (a,b,_) -> (a * b) )

[<Fact>]
let ans()=
  let ans = problem27()
  printfn "%i" ans
  ans |> should equal -59231
