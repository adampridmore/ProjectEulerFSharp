#r @"..\packages\FSPowerPack.Parallel.Seq.Community.3.0.0.0\Lib\Net40\FSharp.PowerPack.Parallel.Seq.dll"
#load "primes.fs"
#load "Seq.fs"

open Microsoft.FSharp.Collections

Seq.unfold (fun x -> Some(x+1,x+1)) 0
|> Seq.take 10
|> Seq.iter (printf "%d\r\n")

Seq.initInfinite (fun x -> x + 1)
|> Seq.take 10
|> Seq.sum
|> printfn "%A"
 

Seq.unfoldInf (fun x -> x + 1, x + 1) 0
|> Seq.take 10
|> Seq.sum
|> printfn "%A"

