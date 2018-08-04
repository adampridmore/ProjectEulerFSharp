open System
open System.Reflection
open ProjectEuler
open FSharp.Collections.ParallelSeq

type s = Skipped

let timespanToString (ts:TimeSpan) =
    match ts with
    | ts when ts.TotalSeconds >= 2. -> sprintf "%4d seconds" (ts.TotalSeconds |> int)
    | ts -> sprintf "%4d ms     " (ts.TotalMilliseconds |> int)

let printAns (ans:Object) =
    match ans with
    | null -> "null"
    | ans -> ans.ToString()

let invokeProblem (meth:MethodInfo) =
    try 
        meth.Invoke(null,[||])
    with | ex ->
        System.Diagnostics.Debug.WriteLine(meth.Name)
        System.Diagnostics.Debug.WriteLine(ex) 
        ex.InnerException.Message :> Object

let executeProblem (meth:MethodInfo) = 
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let ans = meth |> invokeProblem
    (sw.Elapsed, ans)

let getProblemResultText (meth:MethodInfo, problem:Problem) =
    match problem.Speed with
    | Speed.Fast -> (executeProblem meth)
    | Speed.Slow -> (System.TimeSpan.Zero, "** Slow ** Skipped" :> Object)
    | sp -> failwith (sprintf "Unknown speed: %A" sp)
    |> (fun (elapsed, ans ) -> 
            sprintf "Problem %03d %s -   %s " problem.Number (elapsed |> timespanToString) (ans |> printAns) )

[<EntryPoint>]
let main argv = 
    let sw = System.Diagnostics.Stopwatch.StartNew()

    let run() = 
        // Parallel
        ProjectEuler.getAllProblems
        |> PSeq.mapi (fun i fn -> i , fn |> getProblemResultText )
        |> Seq.sortBy fst
        |> Seq.iter (fun (_ , text) -> printfn "%s" text)

        // ProjectEuler.getAllProblems
        // |> Seq.map getProblemResultText
        // |> Seq.iter (printfn "%s")
  
    // seq{0..5}  |> Seq.iter (fun _ -> run())
    run()
    
    printfn "Duration: %fms" sw.Elapsed.TotalMilliseconds

    0
