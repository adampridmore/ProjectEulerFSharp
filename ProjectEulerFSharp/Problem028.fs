module problem028

open FsUnit
open NUnit.Framework

type Side = Top | Bottom | Left | Right

let printBlocks blocks =
    let newLine = System.Environment.NewLine

    let printLine line = 
        (line
        |> Array.map (fun cell -> sprintf "%5d " cell )
        |> Array.reduce (+) ) + newLine 

    blocks
    |> Array.map printLine
    |> Array.reduce (+)
    |> (printf "%s")

let blockWidth (block: int array array) = (block.[0]) |> Seq.length 

let nextBlock (prevBlock : int array array) side =
    let nextSide = side |> function 
                           | Top -> Right
                           | Right -> Bottom
                           | Bottom -> Left
                           | Left -> Top

    let maxValue = prevBlock |> Seq.collect (id) |> Seq.max
    
    let prevWidth = prevBlock |> blockWidth
    
    let nextNumbers = seq{(maxValue+1)..(prevWidth+1+1+maxValue)} |> Seq.toArray

    let appendRight() = 
        prevBlock 
        |> Array.mapi (fun i line -> 
            let value = nextNumbers.[i]
            Array.append line [|value|] )

    let appendLeft() = 
        prevBlock 
        |> Array.mapi (fun i line -> 
            let value = nextNumbers.[prevWidth - i - 1]
            Array.append [|value|] line )

    let appendBottom() =
        let lineLength = prevBlock.[0].Length
        Array.append prevBlock [|(nextNumbers |> Array.take prevWidth |> Array.rev)|]
 
    let appendTop() =
        Array.append [|(nextNumbers |> Array.take prevWidth) |] prevBlock

    let getNextSide = 
        function
        | Right -> appendRight()
        | Left -> appendLeft()
        | Bottom -> appendBottom()
        | Top -> appendTop()

    ( (getNextSide nextSide), nextSide)
    
let seedBlock = 
    [|
        [|1|]
    |]

let createBlock width = 
    Seq.unfold (fun (prevBlock, prevSide) -> let (block, side) = nextBlock prevBlock prevSide
                                             Some((block, side), (block, side) ) ) (seedBlock,Top)
    |> Seq.takeWhile (fun (block, _) -> (block |> blockWidth) <= width) 
    |> Seq.last
    |> (fun (block, _) -> block)

let sumDiagonals block =
    let width = block |> blockWidth
     
    (seq{0..(width-1)} 
    |> Seq.map (fun i -> block.[i].[i] + block.[(width-1-i)].[i] ) 
    |> Seq.sum) - 1
    
let problem28() = 
    let width = 1001
    createBlock width |> sumDiagonals

[<Test>]
let ans() = 
    problem28() |> should equal 669171001
 // 669171001




