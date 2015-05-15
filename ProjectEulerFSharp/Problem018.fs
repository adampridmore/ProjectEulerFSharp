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

type TreeNode = 
  | Node of int * TreeNode * TreeNode
  | Leaf of int

let rec parseLines lines =
  match lines with
  | [] -> Seq.empty
  | [row] -> row |> Seq.map (fun i -> Leaf(i))
  | row::rest -> 
        let nextRow = parseLines rest
        Seq.zip row (nextRow |> Seq.pairwise)
        |> Seq.map (fun (v,(l,r)) -> Node(v,l,r ))

let parseTextToTree text =
  let nonWhiteSpace text = not (System.String.IsNullOrWhiteSpace(text))

  text
  |> stringToLines
  |> Seq.filter nonWhiteSpace
  |> Seq.map textLineToNumbers
  |> Seq.toList
  |> parseLines 
  |> Seq.head

let rec getDepth tree  = 
  match tree with
  | Leaf(_) -> 1
  | Node(v,l,_) -> 1 + getDepth(l)
let rec pow x y = 
  match y with
  | 0 -> 1
  | 1 -> x
  | n -> x * (pow x n-1)

type direction = |Left|Right

let intToDirections i length =
  let sequenceOf item length =
    Seq.init length (fun _ -> item)

  let rec intToDirectionsInternal i =
    match i with
    | 0 -> [Left]
    | 1 -> [Right]
    | _ -> match (i%2) with
            | 0 -> Left::intToDirectionsInternal (i/2)
            | 1 -> Right::intToDirectionsInternal (i/2)
            | _ -> failwith "Error"

  match length with
  | 0 -> []
  | _ ->  
    let digits = (intToDirectionsInternal i) |> Seq.toList |> List.rev |> List.toSeq
    let paddingLength = length - (Seq.length digits)
    let padding = sequenceOf Left paddingLength
    Seq.concat [padding;digits] |> List.ofSeq

let rec totalDirections tree directions =
  match tree, directions with
  | Leaf(v),_ -> v
  | Node(v,l,r), nextDir::remainingDirs -> 
                          v + match nextDir with
                              |Left -> totalDirections l remainingDirs
                              |Right -> totalDirections r remainingDirs
  | _ -> failwith "Error"
  
let solver treeText = 
  let tree = treeText |> parseTextToTree
  let depth = (tree |> getDepth)

  Seq.init (pow 2 (depth-1)) (fun i -> i)
  |> Seq.map (fun i -> intToDirections i (depth - 1))
  |> Seq.map (fun directions -> directions |> (totalDirections tree))
  |> Seq.max
 
let problem18 = 
  solver p2

[<Test>]
let ``2 to the power of 3 is 8``()=
  pow 2 3 |> should equal 8

[<Test>]
let ``0 to the power of anything is 0``()=
  pow 0 3 |> should equal 0

[<Test>]
let ``anything to the power of 0 is 1``()=
  pow 3 0 |> should equal 1

[<Test>]
let ans()=
  let ans = problem18
  printfn "%i" ans
  ans |> should equal 1074

[<Test>]
let ``get tree depth``()=
  let text = @"
   1
  2 3
 4 5 6"
  text |> parseTextToTree |> getDepth |> should equal 3

[<Test>]
let ``parse text to tree``()=
  let text = @"
   1
  2 3"
  let tree = parseTextToTree text

  let b = Leaf(2)
  let c = Leaf(3)
  let a = Node(1, b,c)

  tree |> should equal a

[<Test>]
let ``int to directions for length``()=
  intToDirections 0 0 |> should equal []
  intToDirections 0 1 |> should equal [Left]
  intToDirections 1 1 |> should equal [Right]
  intToDirections 0 2 |> should equal [Left;Left]
  intToDirections 1 2 |> should equal [Left;Right]
  intToDirections 2 2 |> should equal [Right;Left]
  intToDirections 3 2 |> should equal [Right;Right]

[<Test>]
let ``simple pyramid solution``()=
  solver p1 |> should equal 23

[<Test>]
let scratch()=
  let p3 = @"
       3
      7 4
     2 4 6
    8 5 9 3
   1 2 3 4 5
"
  let simpleTree = "1\r\n2 3"

  let tree = p2 |> parseTextToTree
  let depth = (tree |> getDepth)

  printfn "Depth %i" depth

  Seq.init (pow 2 (depth-1)) (fun i -> i)
  |> Seq.map (fun i -> intToDirections i (depth - 1))
  |> Seq.map (fun directions -> directions, directions |> (totalDirections tree))
  //|> Seq.iter (fun (dir, total) -> printfn "%i %A" total dir)
  |> Seq.maxBy (fun (_, total) -> total)
  |> (printfn "%A")
