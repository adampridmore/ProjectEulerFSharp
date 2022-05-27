module ProjectEuler
open System
open System.Reflection

type Speed = |Fast=0|Slow=1
type NoAnswer = |NotSolved
    
[<AttributeUsageAttribute(AttributeTargets.Method)>]
type Problem (number:int, description: String)=
    new(number:int) = Problem(number, "NoDescription")

    [<DefaultValue>] val mutable Name : string
    [<DefaultValue>] val mutable Speed : Speed
    inherit Attribute()
    member this.Number = number
    member this.Description = description

let parseAttributeToNumber (att:Attribute) = 
       (att :?> Problem).Number

let getAllProblems = 
    let getProblemAttribute(meth:MethodInfo) = 
        match meth.GetCustomAttribute(typeof<Problem>) with
        | null -> None
        | att -> Some(att :?> Problem)

    System.Reflection.Assembly.GetExecutingAssembly().GetTypes()
    |> Seq.collect (fun t -> t.GetMethods())
    |> Seq.map (fun meth -> (meth, meth |> getProblemAttribute) )
    |> Seq.filter (fun (_, problemAttribute) -> problemAttribute.IsSome)
    |> Seq.map (fun (meth, problemAttribute) -> (meth, problemAttribute.Value))
    |> Seq.sortBy (fun (meth, problemAttribute) -> problemAttribute.Number)


