module problem029

open FsUnit.Xunit
open Xunit

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

[<ProjectEuler.Problem(29,"Distinct powers")>]    
let problem29() = 
    solver 2 100

[<Fact>]
let ans() = 
    problem29() |> should equal 9183





