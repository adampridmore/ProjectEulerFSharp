#r @"..\packages\FSPowerPack.Parallel.Seq.Community.3.0.0.0\Lib\Net40\FSharp.PowerPack.Parallel.Seq.dll"
#r @"..\packages\FsUnit.1.4.0.0\lib\net45\FsUnit.NUnit.dll"
#r @"..\packages\NUnit.2.6.4\lib\nunit.framework.dll"
#r @"C:\Program Files (x86)\Microsoft Visual Studio 14.0\Common7\IDE\ReferenceAssemblies\v2.0\Microsoft.VisualStudio.QualityTools.UnitTestFramework.dll"

open NUnit.Framework
open FsUnit.TopLevelOperators
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestMethod>]
let ans() = 
    2 |> should equal 669171001
//
//#load ".\problem028.fs"
//
//open problem028
//
//let printBlocks blocks =
//    let newLine = System.Environment.NewLine
//
//    let printLine line = 
//        (line
//        |> Array.map (fun cell -> sprintf "%5d " cell )
//        |> Array.reduce (+) ) + newLine 
//
//    blocks
//    |> Array.map printLine
//    |> Array.reduce (+)
//    |> (printf "%s")
//
//let blockWidth (block: int array array) = (block.[0]) |> Seq.length 
//
//let nextBlock (prevBlock : int array array) side =
//    let nextSide = side |> function 
//                           | Top -> Right
//                           | Right -> Bottom
//                           | Bottom -> Left
//                           | Left -> Top
//
//    let maxValue = prevBlock |> Seq.collect (id) |> Seq.max
//    
//    let prevWidth = prevBlock |> blockWidth
//    
//    let nextNumbers = seq{(maxValue+1)..(prevWidth+1+1+maxValue)} |> Seq.toArray
//
//    let appendRight() = 
//        prevBlock 
//        |> Array.mapi (fun i line -> 
//            let value = nextNumbers.[i]
//            Array.append line [|value|] )
//
//    let appendLeft() = 
//        prevBlock 
//        |> Array.mapi (fun i line -> 
//            let value = nextNumbers.[prevWidth - i - 1]
//            Array.append [|value|] line )
//
//    let appendBottom() =
//        let lineLength = prevBlock.[0].Length
//        Array.append prevBlock [|(nextNumbers |> Array.take prevWidth |> Array.rev)|]
// 
//    let appendTop() =
//        Array.append [|(nextNumbers |> Array.take prevWidth) |] prevBlock
//
//    let getNextSide = 
//        function
//        | Right -> appendRight()
//        | Left -> appendLeft()
//        | Bottom -> appendBottom()
//        | Top -> appendTop()
//
//    ( (getNextSide nextSide), nextSide)
//    
////let text = 
////    """
////    21 22 23 24 25
////    20  7  8  9 10
////    19  6  1  2 11
////    18  5  4  3 12
////    17 16 15 14 13"""
//
////nextBlock blocks Right
//
//// #time 
//
//let seedBlock = 
//    [|
//        [|1|]
//    |]
//
//let width = 1001
//let blocks =
//    Seq.unfold (fun (prevBlock, prevSide) -> let (block, side) = nextBlock prevBlock prevSide
//                                             Some((block, side), (block, side) ) ) (seedBlock,Top)
//    |> Seq.takeWhile (fun (block, _) -> (block |> blockWidth) <= width) 
//    |> Seq.last
//    |> (fun (block, _) -> block)
//
//blocks |> printBlocks
//
//let subOne x = x - 1
//
//let topLeftBottomRight = 
//    seq{0..(width-1)} 
//    |> Seq.map (fun i -> blocks.[i].[i] + blocks.[(width-1-i)].[i] ) 
//    |> Seq.sum
//    |> subOne
//
// // 669171001
//
//
//
//
