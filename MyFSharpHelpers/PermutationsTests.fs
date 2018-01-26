module PermutationsTests

open Xunit
open FsUnit.Xunit

open Permutations

[<Fact>]
let ``get all permutations``() = 
    [1;2;3]
    |> getAllPerms
    |> Seq.map (fun x -> x |> printfn "%A"; x)
    |> Seq.toList
    |> should equal [
        [1;2;3]
        [2;1;3]
        [2;3;1]
        [1;3;2]
        [3;1;2]
        [3;2;1]
        ]


[<Fact>]
let ``get all permutations with dup values``() = 
    [1;1;2]
    |> getAllPerms
    |> Seq.map (fun x -> x |> printfn "%A"; x)
    |> Seq.toList
    |> should equal [
        [1;1;2]
        [1;2;1]
        [2;1;1]
        ]

//[<Fact>]
//let ``get lots of permutations``() = 
//
//// TODO - Add stopwatch
//    [1;1;2;2;3;3;4;4;5;5]
//    |> getAllPerms
//    |> Seq.truncate 1000
//    ()
