﻿module textUtils

open Xunit
open FsUnit.Xunit

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

[<Fact>]
let ``"01 02 03" to numbers``() =
  "01 2 03" |> textLineToNumbers |> Seq.toList |> should equal [1;2;3]

[<Fact>]
let ``char to int for 3``()=
  toInt '3' |> should equal 3
  
[<Fact>]
let ``number to string``()=
  123 |> toString |> should equal "123"

[<Fact>]
let ``number to char array``()=
  123 |> toCharArray |> Seq.toList |> should equal ['1';'2';'3']

[<Fact>]
let ``split lines test``()=

  let text = "hello
world"

  text |> stringToLines |> should equal ["hello";"world"]

[<Fact>]
let ``trim lines``()=
  [" hello "; " world "] |> trimLines |> Seq.toList |> should equal ["hello";"world"]
