#r @"..\packages\FSPowerPack.Parallel.Seq.Community.3.0.0.0\Lib\Net40\FSharp.PowerPack.Parallel.Seq.dll"

let f a b = 
    System.Math.Pow(float a,float b) |> bigint

let min = 2
let max = 100

seq{
    for a in min..max do
        for b in min..max do
            yield (a,b) 
}
|> Seq.map (fun (a,b) -> f a b)
|> Seq.distinct
|> Seq.length
|> printfn "%d"
