#r @"..\packages\FSPowerPack.Parallel.Seq.Community.3.0.0.0\Lib\Net40\FSharp.PowerPack.Parallel.Seq.dll"

let add x y = x + y
let addOne = add 1
let product x y = x * y

let f = addOne >> product

f 10 20

