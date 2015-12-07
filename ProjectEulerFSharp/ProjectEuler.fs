module ProjectEuler
open System
open System.Reflection

//type Speed = |Fast|Slow
type NoAnswer = |NotSolved
    
[<AttributeUsageAttribute(AttributeTargets.Method)>]
type Problem (number:int) =
//    [<DefaultValue>] val mutable Name : string
//    [<DefaultValue>] val mutable Speed : Speed
    inherit Attribute()
    member this.Number = number
    //    member this.Speed = match speed with 
//                        | None -> Fast 
//                        | Some(speed) -> speed


let parseAttributeToNumber (att:Attribute) = 
       (att :?> Problem).Number

let getAllProblems = 
    let getProblemNumberAttribute(meth:MethodInfo) = 
        match meth.GetCustomAttribute(typeof<Problem>) with
        | null -> None
        | att -> Some(att |> parseAttributeToNumber)

    let assembly = System.Reflection.Assembly.GetExecutingAssembly()
    let types = assembly.GetTypes()
    
    types 
    |> Seq.collect (fun t -> t.GetMethods())
    |> Seq.map (fun meth -> (meth, meth |> getProblemNumberAttribute) )
    |> Seq.filter (fun (_, isProblem) -> isProblem.IsSome)
    |> Seq.map (fun (meth, isProblem) -> (meth, isProblem.Value))
    |> Seq.sortBy (fun (meth, problemNumber) -> problemNumber)


