module problem028

open FsUnit.Xunit
open Xunit

let addOne x = 
    x + 1

let solver width = 
    let action (value, inc, fourBatch) = 
        let next =  match fourBatch with
                    | x when x < 3 ->   (value+inc, inc, fourBatch+1)
                    | _ -> (value + inc, (inc+2), 0)
        let (nextValue, _,_) = next
        (nextValue,next)

    let max = width*width
    Seq.unfoldInf action (1,2,0)
    |> Seq.takeWhile (fun x -> x <= max)
    |> Seq.sum
    |> addOne

[<ProjectEuler.Problem(28)>]
let problem28() = 
    solver 1001

[<Fact>]
let ans() = 
    problem28() |> should equal 669171001
 // 669171001




