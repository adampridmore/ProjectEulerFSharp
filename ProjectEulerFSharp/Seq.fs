[<RequireQualifiedAccess>]
module Seq

let unfoldInf generator = Seq.unfold (generator >> Some)
