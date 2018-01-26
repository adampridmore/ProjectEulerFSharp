module problem035

open FSharp.Collections.ParallelSeq
open primes
open FsUnit.Xunit
open Xunit

let toCharArray (s:string) = s.ToCharArray()

let rotate (list:List<'a>) = list.Tail @ [list.Head]

let getRotations lst =
    let rec getAll lst i = 
        match i with 
        | 0 -> [] 
        | _ -> lst :: (getAll (rotate lst) (i - 1))
    getAll lst (List.length lst)

let parseInt = System.Int32.Parse

let solver n = 
    let primeSet = primesSequence |> Seq.takeWhile (fun x -> x < n) |> Set.ofSeq

    let isCircularPrime n = 
        n
        |> string 
        |> toCharArray 
        |> Array.toList
        |> getRotations
        |> Seq.map (fun rot -> rot |> Seq.map string |> Seq.reduce(+) |> parseInt)
        |> Seq.exists  (fun x -> not (primeSet.Contains(x)))
        |> not
        
    seq{2..n-1}
    |> PSeq.filter isCircularPrime
    |> Seq.length

[<ProjectEuler.Problem(35)>]
let problem035() = solver 1000000

[<Fact>]
let ``number of circular primes below 100``() = 
    100 |> solver |> should equal 13

[<Fact>]
let solverTest() = 
    problem035() |> (printfn "%d")

