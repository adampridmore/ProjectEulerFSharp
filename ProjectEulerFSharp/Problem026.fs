module problem026

open Xunit
open FsUnit.Xunit

//A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with denominators 2 to 10 are given:
//
//1/2	= 	0.5
//1/3	= 	0.(3)
//1/4	= 	0.25
//1/5	= 	0.2
//1/6	= 	0.1(6)
//1/7	= 	0.(142857)
//1/8	= 	0.125
//1/9	= 	0.(1)
//1/10	= 	0.1
//Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit recurring cycle.
//
//Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.

[<ProjectEuler.Problem(26,"Reciprocal cycles")>]
let problem026() = 
    // Stolen from here:
    // https://github.com/kerams/project-euler-fsharp/blob/master/project-euler-fsharp/Problems21to30.fs
    // Alternative (but same implementaion) explained here:
    // https://theburningmonk.com/2010/09/project-euler-problem-26-solution/
    let repeatingDigitLength n =
        let rec divide remainder digits =
            let rem = remainder * 10
            match rem % n with
            | 0 -> 0
            | r -> let d = rem / n
                   match digits |> List.tryFindIndex ((=) (rem, d)) with
                   | Some(i) -> i + 1 
                   | None -> divide r ((rem, d) :: digits)  

        divide 1 []
    
    [| 1 .. 999 |]
    |> Array.Parallel.mapi (fun i n -> repeatingDigitLength n, i)
    |> Array.maxBy fst
    |> (snd >> (+) 1)



[<Fact>]
let solverTest() = 
    problem026() |> should equal 983
