﻿open problem001
open problem002
open problem003
open problem004
open problem005
open problem006
open problem007
open problem008
open problem009
open problem010
open problem011
open problem012
open problem013
open problem014
open problem015
open problem016
open problem017
open problem018
open problem019

open System

type s = Skipped

[<EntryPoint>]
let main argv = 
  let problems : System.Object list = [
    problem1 ;
    problem2 ;
    problem3 ;
    problem4 ;
    problem5 ;
    problem6 ;
    problem7 ;
    problem8 ;
    problem9 ;
    problem10 ;
    problem11 ;
    problem12 ;
    problem13 ;
    Skipped ; //problem14;
    problem15 ;
    problem16;
    problem17;
    problem18;
    problem19
  ]

  let sw = System.Diagnostics.Stopwatch.StartNew()

  problems 
  |> Seq.zip (Seq.initInfinite (fun i->i+1))
  |> Seq.iter (fun (i, problem) -> printfn "Problem %3i Answer = %A" i problem)
    
  printfn "Total duration %fms" sw.Elapsed.TotalMilliseconds

  0

