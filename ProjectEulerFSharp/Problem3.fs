module Problem3

open primes
open NUnit.Framework
open FsUnit

//The prime factors of 13195 are 5, 7, 13 and 29.
//
//What is the largest prime factor of the number 600851475143 ?

let isXFactorOf (x:int64) (y:int64) =
  y % x = 0L

let findFirstFactor x = 
  if x <= 2L then x 
  else
    seq{2L..x}
    |> Seq.filter (fun (i:int64) -> isXFactorOf i x)
    |> Seq.head

// not sure how to return the last item and terminate the fold. Uses -1 as a magic state to terminate the unfold. Ugly.
let problem3Solver num =
  Seq.unfold (fun (state) -> 
    let ans = findFirstFactor state
    match ans with 
    | x when x = -1L -> None
    | x when x >= state -> Some(ans, -1L)
    | _ -> Some(ans, state / ans)
  ) num
  |> Seq.last
  
let problem3 =
  let numToTry = 600851475143L
  problem3Solver numToTry

[<Test>]
let ``13195 should equal 29``() =
  problem3Solver 13195L |> should equal 29

[<Test>]
let ``10 is a factor of 50``() =
  isXFactorOf 10L 50L |> should equal true

[<Test>]
let ``11 is not a factor of 50``() =
  isXFactorOf 11L 50L |> should equal false

[<Test>]
let ``first factor of 10 is 2 ``()=
  findFirstFactor 10L |> should equal 2

[<Test>]
let ``first factor of 25 is 5 ``()=
  findFirstFactor 25L |> should equal 5

[<Test>]
let ``answer``() = 
  let answer = problem3
  answer |> should equal 6857
  printfn "%i" answer  
