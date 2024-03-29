﻿module problem024

open Xunit
open FsUnit.Xunit
open math
open textUtils
open System.Threading


//Lexicographic permutations
//Problem 24
//A permutation is an ordered arrangement of objects. For example, 3124 is one 
//possible permutation of the digits 1, 2, 3 and 4. If all of the permutations 
//are listed numerically or alphabetically, we call it lexicographic order. The 
//lexicographic permutations of 0, 1 and 2 are:
//
//012   021   102   120   201   210
//
//What is the millionth lexicographic permutation of 
//the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?

let rec permutation list =
  let getCombinationsThatStartWith item =
    let otherItems = list |> List.filter (fun x -> x <> item)
    let innerCombinations = permutation otherItems
    innerCombinations |> List.map (fun combination -> item :: combination)
    
  match list with
  | [] -> []
  | [one] ->  [[one]]
  | [fst;snd] -> [[fst;snd];[snd;fst]]
  | _ ->  list |> List.collect getCombinationsThatStartWith

let arrayToText a = 
  a 
  |> Seq.map toString
  |> Seq.reduce (+)
  
let solver index numbers = 
  numbers
  |> Seq.toList
  |> permutation
  |> Seq.skip index
  |> Seq.head
  |> arrayToText
  |> toInt64

[<ProjectEuler.Problem(24,"Lexicographic permutations", Speed=ProjectEuler.Speed.Slow)>]
let problem24() = 
  seq{0..9} |> solver (1000000-1) // Zero indexed, so -1 for millionth

[<Fact>]
let ``empty list``()=
  [] |>  permutation |> Seq.toList |> should equal (List.empty<List<'T>>)

[<Fact>]
let ``single item``()=
  [1] |>  permutation |> should equal [[1]]

[<Fact>]
let ``two items``()=
  [1;2] |>  permutation |> should equal [[1;2];[2;1]]

[<Fact>]
let ``three items``()=
  [1;2;3] |>  permutation |> Seq.iter (fun x -> printfn "%A" x)
  [1;2;3] |>  permutation |> should equal [
                                            [1;2;3];[1;3;2]; 
                                            [2;1;3];[2;3;1];
                                            [3;1;2];[3;2;1] 
                                          ]

[<Fact>]
let ``solver for two ``()=
  [1;2] |> solver 0 |> should equal 12L
  [1;2] |> solver 1 |> should equal 21L

//[<Fact>]
//let ``ans``()=
//  let ans = problem24()
//  
//  ans |> printfn "%i"
//  ans |> should equal 2783915460L
