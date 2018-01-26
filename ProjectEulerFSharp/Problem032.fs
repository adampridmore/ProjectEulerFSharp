module problem032

open FsUnit.Xunit
open Xunit

let stringToOrderedArray (s:string) = 
    s.ToCharArray() |> Array.sort

let toChar (x:int) = 
    System.Convert.ToChar(x.ToString())

let charMatch = seq{1..9} |> Seq.map toChar |> Seq.toArray

let isPandigital x y z = 
    let chars = (x.ToString() + y.ToString() + z.ToString())
                |> stringToOrderedArray
    
    chars = charMatch

[<ProjectEuler.Problem(32)>]
let problem32() = 
    seq{
        for x in 1..2000 do
            for y in 1..2000 do
             yield (x,y)
    }
    |> Seq.map (fun (x,y) -> let ans = x*y 
                             (ans, isPandigital x y ans))
    |> Seq.filter (fun (_, isPandigital) -> isPandigital)
    |> Seq.distinctBy (fun (ans,_) -> ans)
    |> Seq.sumBy (fun (ans,_) -> ans)

[<Fact>]
let ans()=
    problem32() |> should equal 45228
