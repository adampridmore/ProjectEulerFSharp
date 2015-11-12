//let action =
//    function
//    | _, false -> None
//    | state, _ when state = 20 -> Some(state, (20, false))
//    | state, _-> Some(state, (state+1, true))
//Seq.unfold action (0, true)
//|> Seq.iter (printfn "%d")
//

//Seq.initInfinite id
//|> Seq.takeWhile (fun x -> x < 100)
////|> Seq.takeWhile ( > 100 )
//|> Seq.iter (printfn "%d")
//
//Seq.initInfinite id 
//|> Seq.truncate 100
//|> Seq.iter (printfn "%d")

let double x = x * 2
let addOne x = x + 1
let fn = Seq.take 100 >> Seq.map double >> Seq.map addOne >> Seq.iter (printfn "%d")
Seq.initInfinite id |> fn
//|> Seq.take 100
//|> Seq.map double
//|> Seq.map addOne 

double (addOne (double 10))
10 |> double |> addOne |> double
