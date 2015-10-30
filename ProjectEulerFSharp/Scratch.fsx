open System.Numerics

let a = bigint.One
let b = bigint 20

let c = a - bigint 1

let repeatCycleLength (n:bigint) = 
  (bigint 10) ** (n - bigint.One)

  //((bigint 10 ** (n - bigint.One)) - bigint.One) / n
 