#r @"..\packages\FSPowerPack.Parallel.Seq.Community.3.0.0.0\Lib\Net40\FSharp.PowerPack.Parallel.Seq.dll"

let stringToOrderedArray (s:string) = 
    s.ToCharArray() |> Array.sort

let toChar (x:int) = 
    System.Convert.ToChar(x.ToString())

let charMatch = seq{1..9} |> Seq.map toChar |> Seq.toArray

let isPandigital x y z = 
    let chars = (x.ToString() + y.ToString() + z.ToString())
                |> stringToOrderedArray
    
    chars = charMatch

seq{
    for x in 1..2000 do
        for y in 1..2000 do
         yield (x,y)
}
|> Seq.map (fun (x,y) -> let ans = x*y 
                         (x,y, ans, isPandigital x y ans))
|> Seq.filter (fun (_,_,_, is) -> is)
|> Seq.distinctBy (fun (_,_,ans,_) -> ans)
|> Seq.map (fun (x,y,ans,is) -> printfn "%dx%d=%d" x y ans; (x,y,ans,is))
|> Seq.sumBy (fun (_,_,ans,_) -> ans)

