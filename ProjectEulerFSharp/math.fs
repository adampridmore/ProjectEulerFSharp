module math

open NUnit.Framework
open FsUnit

let pow x y = 
  let rec powInt x y acc =
    match y with
    | 0 -> 1 
    | 1 -> x * acc 
    | n -> let newAcc = x * acc
           powInt x (y-1) newAcc

  powInt x y 1

[<Test>]
let ``pow: 2 to the power of 3 is 8``()=
  pow 2 3 |> should equal 8

[<Test>]
let ``pow: 1 to the power of 3 is 1``()=
  pow 1 3 |> should equal 1

[<Test>]
let ``pow: 0 to the power of anything is 0``()=
  pow 0 3 |> should equal 0

[<Test>]
let ``pow: anything to the power of 0 is 1``()=
  pow 3 0 |> should equal 1

[<Test>]
let ``pow: 1 to the power of a large is 1``()=
  pow 1 1000000 |> should equal 1
  
