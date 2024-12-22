(* ::Package:: *)

(* ::Text:: *)
(*Written December 22nd, 2024.*)

day = 21;
inputPath = 
  FileNameJoin[{NotebookDirectory[], 
    "Day" <> ToString[day] <> "Input.txt"}];

input = Import[inputPath, "List"];

(* Setup *)

mix[n_, v_] := BitXor[n, v];
prune[n_] := Mod[n, 2^24];
step[n_] :=
  Module[{curr},
   curr = n;
   curr = prune[mix[curr, 64 curr]];
   curr = prune[mix[curr, Quotient[curr, 32]]];
   curr = prune[mix[curr, 2048 curr]]
   ];
stepC = Compile[
   {{n, _Integer}},
   Evaluate[step[n]],
   CompilationTarget -> "C",
   Parallelization -> True,
   RuntimeAttributes -> {Listable},
   RuntimeOptions -> "Speed"];

(* Part 1 *)

Sum[Nest[stepC, num, 2000], {num, input}]

(* Part 2 *)

allFuture = Table[Mod[NestList[stepC, num, 2000], 10], {num, input}];
ClearAll@value;
value[n_] := 0;
Do[
 (value[#[[1, 1]]] += #[[1, 2]]) & /@ 
  GatherBy[Thread[
    Partition[Differences[allFuture[[i]]], 4, 1] -> 
     allFuture[[i, 5 ;;]]], First];,
 {i, Length[allFuture]}];
Max[DownValues[value][[;; , 2]]]