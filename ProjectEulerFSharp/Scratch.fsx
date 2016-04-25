#r @"..\packages\FSPowerPack.Parallel.Seq.Community.3.0.0.0\Lib\Net40\FSharp.PowerPack.Parallel.Seq.dll"
#load "primes.fs"

open Microsoft.FSharp.Collections

open primes

//seq{0..1000000} 
//|> Seq.filter isPrime
////|> Seq.iter (printfn "%A")
//|> Seq.length

primesSequence |> Seq.takeWhile (fun x -> x < 1000000) |> Seq.length