module problem022

open Xunit
open FsUnit.Xunit

//Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over five-thousand first names, begin by sorting it into alphabetical order. Then working out the alphabetical value for each name, multiply this value by its alphabetical position in the list to obtain a name score.
//
//For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain a score of 938 × 53 = 49714.
//
//What is the total of all the name scores in the file?

let removeQuotes (text:string) =
  text.Replace("\"", "")
  
let charToScore (c:char) =
  int32(System.Char.ToUpper(c)) - 64
  
  
let scoreName (name:string) = 
  name.ToCharArray()
  |> Seq.map charToScore 
  |> Seq.sum

[<ProjectEuler.Problem(22,"Names scores")>]
let problem22() =
  (resources.loadResourceAsText "ProjectEulerFSharp.p022_names.txt").Split(',')
  |> Seq.map (fun quotedName -> quotedName |> removeQuotes)
  |> Seq.sort
  |> Seq.map scoreName
  |> Seq.mapi (fun i score -> (i+1) * score)
  |> Seq.sum
  
[<Fact>]
let ans()=
  problem22() |> should equal 871198282
  
[<Fact>]
let ``a is scored 1``()=
  charToScore 'a' |> should equal 1

[<Fact>]
let ``A is scored 1``()=
  charToScore 'A' |> should equal 1
  
[<Fact>]
let ``z is scored 26``()=
  charToScore 'z' |> should equal 26

[<Fact>]
let ``COLIN  is scored 53``()=
  "COLIN" |> scoreName |> should equal 53
