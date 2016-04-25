module problem035

open Microsoft.FSharp.Collections
open primes
open FsUnit
open NUnit.Framework

let toCharArray (s:string) = s.ToCharArray()

let rotate (list:List<'a>) = list.Tail @ [list.Head]

let getRotations lst =
    let rec getAll lst i = 
        match i with 
        | 0 -> [] 
        | _ -> lst :: (getAll (rotate lst) (i - 1))
    getAll lst (List.length lst)

let parseInt = System.Int32.Parse

let isCircularPrime n = 
    n
    |> string 
    |> toCharArray 
    |> Array.toList
    |> getRotations
    |> Seq.map (fun rot -> rot |> Seq.map string |> Seq.reduce(+) |> parseInt)
    |> Seq.exists  (fun x -> not (isPrime x))
    |> not

let solver n = 
    seq{2..n-1}
    |> PSeq.filter isCircularPrime
    |> Seq.length

[<ProjectEuler.Problem(35, Speed=ProjectEuler.Speed.Slow)>]
let problem035() = solver 1000000

[<Test>]
let ``197 is circular prime``() = 
    197 |> isCircularPrime |> should equal true

[<Test>]
let ``number of circular primes below 100``() = 
    100 |> solver |> should equal 13

//[<Test>]
//let solverTest() = 
//    problem035() |> (printfn "%d")

