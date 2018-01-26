module Permutations

open FSharp.Collections.ParallelSeq

let rec insertions pre c post =
    seq {
        if List.length post = 0 then
            yield pre @ [c]
        else
            if List.forall (fun x->x<>c) post then
                yield pre@[c]@post
            yield! insertions (pre@[post.Head]) c post.Tail
        }

let rec permutations l =
    seq {
        if List.length l = 1 then
            yield l
        else
            let subperms = permutations l.Tail
            for sub in subperms do
                yield! insertions [] l.Head sub
        }

let getAllPerms = permutations

let numberOfPermutations pipes = 
    let tobigint (x:int) =
        bigint x

    let rec fact x = 
        match x with
        | x when x = bigint.One -> bigint.One
        | x -> x * fact (x - bigint.One)

    let permutations = fact (pipes |> Seq.length |> bigint) 
    let dupesFactor =   pipes 
                        |> Seq.groupBy id
                        |> Seq.map (fun (_,x) -> x |> Seq.length |> tobigint |> fact)
                        |> Seq.reduce (*)

    permutations / dupesFactor




let PGetAllPerms pipes = 
    let getOtherPerms pipe (otherPipes: 'a list ) =
        otherPipes 
        |> getAllPerms
        |> Seq.map (fun perm -> List.concat [[pipe];perm] )

    pipes
    |> PSeq.map (fun pipe -> pipes
                             |> List.filter (fun p -> p <> pipe)
                             |> (fun otherPipes -> otherPipes |> getOtherPerms pipe))
    |> PSeq.collect id