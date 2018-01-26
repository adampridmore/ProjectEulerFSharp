module SeqTests

open Xunit
open FsUnit.Xunit


[<Fact>]
let ``unfod inf test for repating 1's``() =
   Seq.unfoldInf (fun x -> 1,() ) ()
   |> Seq.take 2
   |> Seq.toList
   |> should equal [1;1]
      
[<Fact>]
let ``unfod inf test for incrementing seq``() =
   Seq.unfoldInf (fun a -> a, a+1) 0
   |> Seq.take 2 
   |> Seq.toList
   |> should equal [0;1]
   
[<Fact>]
let ``unfold sum a large seq``() =
   let numberToDo = 2000000

   Seq.unfoldInf (fun a -> 1L, () ) ()
   |> Seq.take numberToDo
   |> Seq.sum
   |> should equal (numberToDo |> int64)

[<Fact>]
let ``Fib seq``() =
    Seq.unfoldInf (fun (a,b) -> (a, (b,a+b))) (0,1)
    |> Seq.take 10
    |> printfn "%A"


