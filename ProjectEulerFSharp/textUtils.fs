module textUtils

open NUnit.Framework
open FsUnit

let stringToLines (str:string) = 
  let split (s:string) = 
    s.Split([|System.Environment.NewLine|], System.StringSplitOptions.RemoveEmptyEntries)

  str |> split |> List.ofSeq
  

let toString x =
   x.ToString()

let toCharArray x = 
  let s = x |> toString
  s.ToCharArray()
 
let toInt x =
  System.Int32.Parse(x.ToString())

let trimLines lines = 
  lines |> Seq.map (fun (s:string) -> s.Trim()) |> Seq.toArray
  
let textLineToNumbers line =
  let split (s:string) = 
    s.Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries)

  line |> split |> Seq.map toInt

[<Test>]
let ``"01 02 03" to numbers``() =
  "01 2 03" |> textLineToNumbers |> should equal [|1;2;3|]

[<Test>]
let ``char to int for 3``()=
  toInt '3' |> should equal 3
  
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

  text |> stringToLines |> should equal [|"hello";"world"|]

[<Test>]
let ``trim lines``()=
  [" hello "; " world "] |> trimLines |> should equal ["hello";"world"]
