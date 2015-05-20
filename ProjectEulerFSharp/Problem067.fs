module problem067

open problem018
open NUnit.Framework
open FsUnit

let problem67 = 
  let r = System.Reflection.Assembly.GetExecutingAssembly().GetManifestResourceStream("p067_triangle.txt")
  let reader = new System.IO.StreamReader(r)
  let text = reader.ReadToEnd()
    
  solver text

[<Test>]
let ans() = 
  let ans = problem67
  printfn "%A" ans
  ans |> should equal 7273