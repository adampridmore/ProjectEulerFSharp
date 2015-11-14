#r @"..\packages\FSPowerPack.Parallel.Seq.Community.3.0.0.0\Lib\Net40\FSharp.PowerPack.Parallel.Seq.dll"

1

3+5+7+9

13+17+21+25

let batchSeq start inc =
    Seq.initInfinite(fun x -> start + x * inc)
    |> Seq.take 4

batchSeq 1 2
|> Seq.iter (printfn "%A")

let solver width = 
    let addOne x = 
        x + 1

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
