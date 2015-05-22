module problem023

open NUnit.Framework
open FsUnit
open math


(*
A perfect number is a number for which the sum of its proper divisors is exactly equal to the number. 
For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 
28 is a perfect number.

A number n is called deficient if the sum of its proper divisors is less than n and it is called 
abundant if this sum exceeds n.

As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be 
written as the sum of two abundant numbers is 24. By mathematical analysis, it can be shown that all 
integers greater than 28123 can be written as the sum of two abundant numbers. However, this upper 
limit cannot be reduced any further by analysis even though it is known that the greatest number 
that cannot be expressed as the sum of two abundant numbers is less than this limit.

Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.
*)

let maxValue = 28123

let removeFromList list itemsToRemove = 
  Seq.concat [
    itemsToRemove |> Seq.map (fun x -> x, true);
    list |> Seq.map (fun x -> x,false)
  ]
  |> Seq.distinctBy (fun (x,_) -> x)
  |> Seq.filter (fun (_,toRemove) -> not toRemove)
  |> Seq.map(fun (x,_) -> x)

let abundantNumbers = 
  Seq.initInfinite(fun i -> i + 1)
  |> Seq.map (fun n -> n, (properDivisorsSum n))
  |> Seq.filter (fun (n, sum) -> sum > n)
  |> Seq.map(fun (n,_) -> n)
                    
let abundantNumbersLimited = 
  abundantNumbers |> Seq.take maxValue |> Seq.toList

let numbersThatAreSumOfTwoAbundantNumbers  = 
  abundantNumbersLimited
  |> Seq.collect (fun num -> 
            abundantNumbersLimited 
            |> Seq.map (fun num2 -> num + num2) 
            |> Seq.takeWhile (fun x -> x <= maxValue))
  |> Seq.distinct
  |> Seq.sort

let problem23 = 
  removeFromList [1..maxValue] numbersThatAreSumOfTwoAbundantNumbers
  |> Seq.sum
 
[<Test>]
let ans()=
  problem23 |> should equal 4179871
  
[<Test>]
let ``first 10 abundant numbers``()=
  abundantNumbers |> Seq.take 10 
  |> should equal [12;18;20;24;30;36;40;42;48;54]

[<Test>]
let ``remove from list``()=
  removeFromList [1;2;3;4;5] [2;4] |> Seq.iter (printfn "%A")

  removeFromList [1;2;3;4;5] [2;4] |> should equal [1;3;5]

[<Test>]
let scratch()=
  ()
//  printfn "Abundent Numbers:"
//  abundantNumbers 
//  |> Seq.takeWhile (fun x -> x <= maxValue) 
//  |> Seq.iter (printfn "%i")

  //numbersThatAreSumOfTwoAbundantNumbers |> Seq.length |> (printfn "Length: %A")

//  printfn "Numbers that are the sum of two abundent numbers:"
//  numbersThatAreSumOfTwoAbundantNumbers |> Seq.iter (printfn "%A")

//  printfn "Numbers that are NOT the sum of two abundent numbers:"
//  removeFromList [1..maxValue] numbersThatAreSumOfTwoAbundantNumbers
//  |> Seq.sum |> printfn "%i"
  // 4179871 yup
