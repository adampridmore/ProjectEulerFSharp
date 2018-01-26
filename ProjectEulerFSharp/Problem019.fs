module problem019

open FsUnit.Xunit
open Xunit
open System

//  You are given the following information, but you may prefer to do some research for yourself.
//
//    1 Jan 1900 was a Monday.
//
//    Thirty days has September,
//    April, June and November.
//    All the rest have thirty-one,
//    Saving February alone,
//    Which has twenty-eight, rain or shine.
//    And on leap years, twenty-nine.
//
//    A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
//
//  How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?

let solver =
  let startDateTime = new DateTime(1901,1,1,0,0,0, DateTimeKind.Utc)
  let endDateTime = new DateTime(2000,12,31,0,0,0,DateTimeKind.Utc)

  let isValid (dt:DateTime) =
    dt.DayOfWeek = DayOfWeek.Sunday && dt.Day = 1

  Seq.initInfinite (fun i-> startDateTime + TimeSpan.FromDays(float(i)))
  |> Seq.takeWhile (fun date -> date <= endDateTime )
  |> Seq.filter isValid
  |> Seq.length

[<ProjectEuler.Problem(19)>]
let problem19() = 
  solver

[<Fact>]
let ans() =
  let n = solver
  printfn "%i" n
  n |> should equal 171