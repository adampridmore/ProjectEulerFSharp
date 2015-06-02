module textUtils

open NUnit.Framework
open FsUnit

let stringToLines (str:string) = 
  let split (s:string) = 
    let newLineChars = [|
      System.Environment.NewLine;
      "\n";
      "\r\n"
    |]

    s.Split(newLineChars, System.StringSplitOptions.RemoveEmptyEntries)

  str |> split |> List.ofSeq
  
let toString x =
   x.ToString()

let toCharArray x = 
  let s = x |> toString
  s.ToCharArray()
 
let toInt x =
  let (res, v) = System.Int32.TryParse(x.ToString());
  match res with 
  | true -> v
  | false -> failwith (sprintf "Invalid int: %A" x)

let toInt64 x =
  let (res, v) = System.Int64.TryParse(x.ToString());
  match res with 
  | true -> v
  | false -> failwith (sprintf "Invalid int: %A" x)

let trimLines lines = 
  lines |> Seq.map (fun (s:string) -> s.Trim()) |> Seq.toArray
  
let textLineToNumbers line =
  let split (s:string) = 
    s.Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries)

  line |> split |> Seq.map toInt

let loadResourceAsText name =
  let assembly = System.Reflection.Assembly.GetExecutingAssembly()
  let stream  = assembly.GetManifestResourceStream(name)
  let reader = new System.IO.StreamReader(stream);
  reader.ReadToEnd()  

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
