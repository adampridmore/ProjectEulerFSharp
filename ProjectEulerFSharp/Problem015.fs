﻿module problem015

open Xunit
open FsUnit.Xunit
open math


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

let solver (i:int32) =
  let n = bigint (i * 2)
  (factorial n) / ( factorial (n/bigint 2) * factorial (n/bigint 2))

[<ProjectEuler.Problem(15,"Lattice paths")>]
let problem15() = solver 20

[<Fact>]
let scratch()=
  let n = 4
  let ans = solver n
  printfn "%O" ans

[<Fact>]
let scratch2()=
  let n = 40
  let ans = solver n
  printfn "%O" ans

[<Fact>]
let ans()=
  let ans = solver 20
  printfn "%O" ans
  ans |> should equal (bigint 137846528820L)
