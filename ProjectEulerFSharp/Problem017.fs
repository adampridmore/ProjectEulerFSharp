module problem017

open NUnit.Framework
open FsUnit

// Number letter counts
// Problem 17
// If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
//
// If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?
//
//
// NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.

let numberAndWord =
   [
    (0,"zero");
    (1,"one");
    (2,"two");
    (3,"three");
    (4,"four");
    (5,"five");
    (6,"six");
    (7,"seven");
    (8,"eight");
    (9,"nine");
    (10,"ten");
    (11,"eleven");
    (12,"twelve");
    (13,"thirteen");
    (14,"fourteen");
    (15,"fifteen");
    (16,"sixteen");
    (17,"seventeen");
    (18,"eighteen");
    (19,"nineteen");
    (20,"twenty");
    (30,"thirty");
    (40,"forty");
    (50,"fifty");
    (60,"sixty");
    (70,"seventy");
    (80,"eighty");
    (90,"ninety");
    (100,"one hundred");
    (200,"two hundred");
    (300,"three hundred");
    (400,"four hundred");
    (500,"five hundred");
    (600,"six hundred");
    (700,"seven hundred");
    (800,"eight hundred");
    (900,"nine hundred");
    (1000,"one thousand");
  ]

let biggestNumberAndWord numberToFind = 
  numberAndWord 
  |> List.rev
  |> Seq.find (fun (number,_) -> numberToFind - number >= 0)

let numberToWords number =
  let andIfNeeded number word =
    let needsPostAnd n = (n % 100 <> 0) && (n / 100 > 0)

    if needsPostAnd number then sprintf "%s and" word
    else word
  
  let action remainder = 
    if remainder = 0 then None
    else
      let num, word = biggestNumberAndWord remainder
      Some(andIfNeeded remainder word, remainder - num)
  
  Seq.unfold action number
  |> Seq.reduce (fun a b -> sprintf "%s %s" a b)

let numberTextLength numberText =
  (string numberText).ToCharArray() 
  |> Seq.filter(fun c -> c <> ' ') 
  |> Seq.length

let solver n = 
  {1..n} 
  |> Seq.map (numberToWords >> numberTextLength)
  |> Seq.sum

[<ProjectEuler.Problem(17)>]
let problem17() = solver 1000

[<Test>]
let ans()=
  let ans = problem17() 
  printfn "%i" ans
  ans |> should equal 21124
      
[<Test>]
let ``biggest number & word for 25``()=
  let n, word = biggestNumberAndWord 25
  n |> should equal 20
  word |> should equal "twenty"

[<Test>]
let ``1``() = numberToWords 1 |> should equal "one"

[<Test>]
let ``19``() = numberToWords 19 |> should equal "nineteen"

[<Test>]
let ``20``() = numberToWords 20 |> should equal "twenty"

[<Test>]
let ``21``() = numberToWords 21 |> should equal "twenty one"

[<Test>]
let ``number text length for 'twenty one' is 9``()=
  numberTextLength "twenty one" |> should equal 9

[<Test>]
let ``342 has 23 letters``()=
  numberToWords 342 |> should equal "three hundred and forty two"
  numberToWords 342 |> numberTextLength |> should equal 23

[<Test>]
let ``115 has 20 letters``()=
  numberToWords 115 |> should equal "one hundred and fifteen"
  numberToWords 115 |> numberTextLength |> should equal 20
