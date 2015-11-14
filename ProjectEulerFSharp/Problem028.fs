module problem028

open FsUnit
open NUnit.Framework

let addOne x = 
    x + 1

let solver width = 
    let action (value, inc, fourBatch) = 
        let next =  match fourBatch with
                    | x when x < 3 ->   (value+inc, inc, fourBatch+1)
                    | _ -> (value + inc, (inc+2), 0)
        let (nextValue, _,_) = next
        Some(nextValue,next)

    let max = width*width
    Seq.unfold action (1,2,0)
    |> Seq.takeWhile (fun x -> x <= max)
    |> Seq.sum
    |> addOne


let problem28() = 
    solver 1001

[<Test>]
let ans() = 
    problem28() |> should equal 669171001
 // 669171001




