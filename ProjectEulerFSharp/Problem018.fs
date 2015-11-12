module problem018

open NUnit.Framework
open FsUnit
open textUtils
open math

//    Maximum path sum I
//    Problem 18
//    By starting at the top of the triangle below and moving to adjacent numbers on the row below, the maximum total from top to bottom is 23.
//
let p1 = @"
       3
      7 4
     2 4 6
    8 5 9 3
"
//
//    That is, 3 + 7 + 4 + 9 = 23.
//
//    Find the maximum total from top to bottom of the triangle below:
//
let p2 = @"              75
                        95 64
                      17 47 82
                    18 35 87 10
                   20 04 82 47 65
                 19 01 23 75 03 34
                88 02 77 73 07 63 67
              99 65 04 28 06 16 70 92
             41 41 26 56 83 40 80 70 33
           41 48 72 33 47 32 37 16 94 29
          53 71 44 65 25 43 91 52 97 51 14
        70 11 33 28 77 73 17 78 39 68 17 57
       91 71 52 38 17 14 91 43 58 50 27 29 48
     63 66 04 68 89 53 67 30 73 16 69 87 40 31
    04 62 98 27 23 09 70 98 73 93 38 53 60 04 23
    "
//
//    NOTE: As there are only 16384 routes, it is possible to solve this problem by trying every route. However, Problem 67, is the same challenge with a triangle containing one-hundred rows; it cannot be solved by brute force, and requires a clever method! ;o)

let nonWhiteSpace text = not (System.String.IsNullOrWhiteSpace(text))
   
let max a b = 
  match a,b with
  | a,b when a > b -> a
  | _,b -> b

// This iterates through a row, a of values pair at a time and picks the max 
let compressLine line =
  match line with
  | [] | [_]  -> line
  | _ ->  line
          |> Seq.pairwise 
          |> Seq.map (fun (a,b) -> max a b)
          |> Seq.toList

let solver treeText =
  let rec processLines (lines:list<list<int>>) = 
    let joinRows = function 
          | [] -> [0]
          | [lastRow] -> lastRow
          | hd::rest -> 
              List.zip (processLines rest) hd 
              |> List.map (fun (a,b) -> a + b)
                
    lines |> joinRows |> compressLine

  treeText
  |> stringToLines
  |> Seq.filter nonWhiteSpace
  |> Seq.map (fun text -> textLineToNumbers text |> Seq.toList)
  |> Seq.toList
  |> processLines
  |> Seq.head
  
let problem18() = 
  solver p2

[<Test>]
let ans()=
  let ans = problem18()
  printfn "%i" ans
  ans |> should equal 1074

[<Test>]
let ``simple pyramid solution``()=
  solver p1 |> should equal 23

[<Test>]
let ``max when a > b should be a``()=
  max 6 5 |> should equal 6

[<Test>]
let ``max when a < b should be b``()=
  max 4 5 |> should equal 5

[<Test>]
let ``compress line picks max of all pairs of values``()=
  [1;2;3;4;5] |> compressLine |> should equal [2;3;4;5]