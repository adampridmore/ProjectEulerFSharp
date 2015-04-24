module problem015

open NUnit.Framework
open FsUnit


//Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.
//
//How many such routes are there through a 20×20 grid?

//RRDD
//RDRD
//RDDR
//DRRD
//DRDR
//DDRR
//
//
//4!/ 2!.2!

let rec factorial (x:bigint) = 
  match x with
  | x when x = bigint.One -> bigint.One
  | x -> x * factorial (x - bigint.One)

let solver (i:int32) =
  let n = bigint (i * 2)
  (factorial n) / ( factorial (n/bigint 2) * factorial (n/bigint 2))

let problem15 =
  solver 20

[<Test>]
let scratch()=
  let n = 4
  let ans = solver n
  printfn "%O" ans

[<Test>]
let scratch2()=
  let n = 40
  let ans = solver n
  printfn "%O" ans

[<Test>]
let ans()=
  let ans = solver 20
  printfn "%O" ans
  ans |> should equal (bigint 137846528820L)