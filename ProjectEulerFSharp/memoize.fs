module memoize

open System.Collections.Generic
open System

let memoize f =
  let dict = new System.Collections.Generic.Dictionary<_,_>()
  fun n ->
      match dict.TryGetValue(n) with
      | (true, v) -> v
      | _ -> let temp = f(n)
             dict.Add(n, temp)
             temp
 
let fn = 
  let fni (x:int) = DateTime.UtcNow
  memoize fni
