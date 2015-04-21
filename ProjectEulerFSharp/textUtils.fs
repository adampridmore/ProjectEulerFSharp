module textUtils

let splitLines (s:string) = 
  List.ofSeq(s.Split([|System.Environment.NewLine|], System.StringSplitOptions.RemoveEmptyEntries))


let toString x =
  x.ToString()

let toCharArray x = 
  let s = x |> toString
  s.ToCharArray()