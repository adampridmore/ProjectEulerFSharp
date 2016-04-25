module primes

let isPrime n = 
    match n with
    | 0 | 1 -> false
    | n -> 
        let upperBound = int (sqrt (float n))
        let hasDivisor =     
            [2..upperBound]
            |> List.exists (fun i -> n % i = 0)
        not hasDivisor

let primes =
  let a = ResizeArray[2]
  let grow() =
    let p0 = a.[a.Count-1]+1
    let b = Array.create p0 true
    for di in a do
      let rec loop i =
        if i<b.Length then
          b.[i] <- false
          loop(i+di)
      let i0 = p0/di*di
      loop(if i0<p0 then i0+di-p0 else i0-p0)
    for i=0 to b.Length-1 do
      if b.[i] then a.Add(p0+i)
  fun n ->
    while n >= a.Count do
      grow()
    a.[n]

let primesSequence = Seq.initInfinite id |> Seq.map primes

let primeFactors i = 
  let rec fac n x a = 
    if x = n then
      x::a
    elif n % x = 0 then 
      fac (n/x) x (x::a)
    else
      fac n (x+1) a

  match i with
  | 0 -> []
  | 1 -> []
  | _ -> fac i 2 []

