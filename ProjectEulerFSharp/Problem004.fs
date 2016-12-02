﻿module problem004

open NUnit.Framework
open FsUnit
open textUtils

//A palindromic number reads the same both ways. The largest palindrome made 
// from the product of two 2-digit numbers is 9009 = 91 × 99.
//
//Find the largest palindrome made from the product of two 3-digit numbers.

let isPalindrome i =
  let characters = i |> toCharArray
  let revCharacters = ( characters |> Array.rev )
  characters = revCharacters
  
let getAllProductsOfNumbersFromTo minNum maxNum =
  seq { 
    for x in minNum..maxNum do 
        for y in minNum..maxNum do 
            yield x*y
  }

[<ProjectEuler.Problem(4)>]
let problem4() = 
  getAllProductsOfNumbersFromTo 900 999
  |> Seq.filter isPalindrome
  |> Seq.max 

[<Test>]
let ``answer``() =
  let ans = problem4()
  printfn "%i" ans
  ans |> should equal 906609

[<Test>]
let ``get all products from 1 to 2``()=
  getAllProductsOfNumbersFromTo 1 2 |> should equal [|1;2;2;4|]

[<Test>]
let ``123 is not a palindrome``() =
  isPalindrome 123 |> should equal false

[<Test>]
let ``1221 is a palindrome``() =
  isPalindrome 1221 |> should equal true
