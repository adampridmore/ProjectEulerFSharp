#r @"..\packages\FSPowerPack.Parallel.Seq.Community.3.0.0.0\Lib\Net40\FSharp.PowerPack.Parallel.Seq.dll"


#load "Permutations.fs"
open FSharp.Collections.ParallelSeq


[0;1;2;3;4;5;6;7;8;9]
//|> PSeq.map (id)
|> Permutations.PGetAllPerms

[0m;1m]
|> Permutations.PGetAllPerms
