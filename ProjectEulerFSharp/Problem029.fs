module problem029

open FsUnit
open NUnit.Framework

let f a b = 
    System.Math.Pow(float a,float b) |> bigint

let solver min max = 
    seq{
        for a in min..max do
            for b in min..max do
                yield (a,b) 
    }
    |> Seq.map (fun (a,b) -> f a b)
    |> Seq.distinct
    |> Seq.length

[<ProjectEuler.Problem(29)>]    
let problem29() = 
    solver 2 100

[<Test>]
let ans() = 
    problem29() |> should equal 9183





