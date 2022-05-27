module problem004

open Xunit
open FsUnit.Xunit
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

[<ProjectEuler.Problem(4, "Largest palindrome product")>]
let problem4() = 
  getAllProductsOfNumbersFromTo 900 999
  |> Seq.filter isPalindrome
  |> Seq.max 

[<Fact>]
let ``answer``() =
  let ans = problem4()
  printfn "%i" ans
  ans |> should equal 906609

[<Fact>]
let ``get all products from 1 to 2``()=
  getAllProductsOfNumbersFromTo 1 2 |> Seq.toList |> should equal [1;2;2;4]

[<Fact>]
let ``123 is not a palindrome``() =
  isPalindrome 123 |> should equal false

[<Fact>]
let ``1221 is a palindrome``() =
  isPalindrome 1221 |> should equal true
