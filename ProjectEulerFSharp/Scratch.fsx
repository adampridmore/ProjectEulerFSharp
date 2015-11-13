#r @"..\packages\FSPowerPack.Parallel.Seq.Community.3.0.0.0\Lib\Net40\FSharp.PowerPack.Parallel.Seq.dll"

#load ".\primes.fs"

open primes
open Microsoft.FSharp.Collections

let sqr x = x * x

let f a b n = (n |> sqr) + (a * n) + b

let abs = 
    function
    | x when x >= 0 -> x
    | x -> -x

let tester a b = 
    Seq.initInfinite id
    |> Seq.map (f a b)
    |> Seq.map abs
    |> Seq.map (fun x -> isPrime x)
    |> Seq.takeWhile id
    |> Seq.length

let minMaxRange = 999

seq{ for a in -minMaxRange..minMaxRange do
       for b in -minMaxRange..minMaxRange do
            yield (a,b)
}
|> PSeq.map (fun (a,b) -> (a,b,(tester a b) ) )
|> Seq.maxBy(fun (_,_,length) -> length)
|> (fun (a,b,l) -> (a,b,l,(a * b)) )

//  Answers is   -59231