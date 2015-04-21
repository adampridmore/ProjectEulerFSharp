module textUtils

open NUnit.Framework
open FsUnit

let splitLines (str:string) = 
  let split (s:string) = 
    s.Split([|System.Environment.NewLine|], System.StringSplitOptions.RemoveEmptyEntries)

  str |> split |> List.ofSeq
  

let toString x =
   x.ToString()

let toCharArray x = 
  let s = x |> toString
  s.ToCharArray()


let trimLines lines = 
  lines |> Seq.map (fun (s:string) -> s.Trim()) |> Seq.toArray
  


[<Test>]
let ``number to string``()=
  123 |> toString |> should equal "123"

[<Test>]
let ``number to char array``()=
  123 |> toString |> should equal [|'1';'2';'3'|]

[<Test>]
let ``split lines test``()=

  let text = "hello
world"

  text |> splitLines |> should equal [|"hello";"world"|]

[<Test>]
let ``trim lines``()=
  [" hello "; " world "] |> trimLines |> should equal ["hello";"world"]
