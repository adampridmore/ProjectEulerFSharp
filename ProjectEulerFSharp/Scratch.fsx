#r @"..\packages\FSPowerPack.Parallel.Seq.Community.3.0.0.0\Lib\Net40\FSharp.PowerPack.Parallel.Seq.dll"

let charToNumber (c:char) = System.Convert.ToInt32(c.ToString()) 
let pow b e = 
    System.Math.Pow(b |> float, e|> float) |> int

let numberToDigits number = 
    number.ToString().ToCharArray()
    |> Seq.map charToNumber

let sumPowerOfDigits number exponent =
    numberToDigits number
    |> Seq.map (fun n -> pow n exponent)
    |> Seq.sum

let isSumPowerOfDigits number exponent =
    (sumPowerOfDigits number exponent) = number

seq{10..999999}
|> Seq.filter(fun x -> isSumPowerOfDigits x 5)
|> Seq.sum

//443839

