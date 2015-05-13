module problem018

open NUnit.Framework
open FsUnit
open textUtils

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
//                         75  
//                        95 64
//                      17 47 82
//                    18 35 87 10
//                   20 04 82 47 65
//                 19 01 23 75 03 34
//                88 02 77 73 07 63 67
//              99 65 04 28 06 16 70 92
//             41 41 26 56 83 40 80 70 33
//           41 48 72 33 47 32 37 16 94 29
//          53 71 44 65 25 43 91 52 97 51 14
//        70 11 33 28 77 73 17 78 39 68 17 57
//       91 71 52 38 17 14 91 43 58 50 27 29 48
//     63 66 04 68 89 53 67 30 73 16 69 87 40 31
//    04 62 98 27 23 09 70 98 73 93 38 53 60 04 23
//
//    NOTE: As there are only 16384 routes, it is possible to solve this problem by trying every route. However, Problem 67, is the same challenge with a triangle containing one-hundred rows; it cannot be solved by brute force, and requires a clever method! ;o)

let problem18 = 18

[<Test>]
let ans()=
  let ans = problem18
  printfn "%i" ans
  ans |> should equal 1



type Tree = 
  | Node of int * Tree * Tree
  | Leaf of int
  
[<Test>]
let scratch()=
  
  let a = Leaf(1)
  let b = Leaf(2)
  let c = Node(3,a,b)

//  printfn "%A" c

  let p1 = @"
       3
      7 4
     2 4 6
    8 5 9 3
  "

  let d1 = Leaf(8)
  let d2 = Leaf(5)
  let d3 = Leaf(9)
  let d4 = Leaf(3)
  let c1 = Node(2,d1,d2)
  let c2 = Node(4,d2,d3)
  let c3 = Node(6,d3,d4)
  let b1 = Node(7,c1,d2)
  let b2 = Node(4,c2,c3)
  let a1 = Node(3,b1,b2)
  
  let nonWhiteSpace text =
    not (System.String.IsNullOrWhiteSpace(text))

  let numberRows = (p1 
    |> stringToLines
    |> Seq.filter nonWhiteSpace
    |> Seq.map textLineToNumbers
    |> Seq.toList
    |> List.rev
  )

  let row4 = numberRows |> Seq.skip 0 |> Seq.head
  let row3= numberRows |> Seq.skip 1 |> Seq.head
  let row2= numberRows |> Seq.skip 2 |> Seq.head
  let row1= numberRows |> Seq.skip 3 |> Seq.head

  let row4Leaf = row4 |> Seq.map (fun i -> Leaf(i))
  
  let row3Node = (row3 
      |> Seq.zip (row4Leaf |> Seq.pairwise)
      |> Seq.map (fun ((l,r),value)-> Node(value, l,r))
    )

  let row2Node = (row2
      |> Seq.zip (row3Node|> Seq.pairwise)
      |> Seq.map (fun ((l,r),value)-> Node(value, l,r))
    )

  let row1Node = (row1
    |> Seq.zip (row2Node|> Seq.pairwise)
    |> Seq.map (fun ((l,r),value)-> Node(value, l,r))
  )

  let parsedNode = row1Node |> Seq.head 
  
  parsedNode |> (printfn "%A")







