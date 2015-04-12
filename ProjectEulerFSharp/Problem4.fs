module Problem4

open NUnit.Framework
open FsUnit

//A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
//
//Find the largest palindrome made from the product of two 3-digit numbers.

let isPalindrome i =
  let numberCharacters = i.ToString().ToCharArray()
  let reversedCharacters = Array.rev(numberCharacters)
  numberCharacters = reversedCharacters

let problem4 = 
  let maxNum = 999
  let nums = (Seq.unfold (fun state -> 
      match state with 
      | 0,0 -> None
      | 0,y -> Some((maxNum * y),(maxNum, y-1))
      | x,y -> Some((x * y),(x - 1, y))
    ) (maxNum, maxNum)
  )

  nums 
  |> Seq.filter (fun i -> isPalindrome i)
  |> Seq.max 

[<Test>]
let ``answer``() =
  let ans = problem4
  printfn "%i" ans
  ans |> should equal 906609

[<Test>]
let ``123 is not a palindrome``() =
  isPalindrome 123 |> should equal false

[<Test>]
let ``1221 is a palindrome``() =
  isPalindrome 1221 |> should equal true
