(* ::Package:: *)

(* ::Text:: *)
(*Written December 18th, 2024.*)

day = 18;
inputPath = 
  FileNameJoin[{NotebookDirectory[], 
    "Day" <> ToString[day] <> "Input.txt"}];

toExpression[inputText_] :=
  Map[
   If[! IntegerQ[#] \[And] 
      StringMatchQ[#, 
       Alternatives["+", "-", ""] ~~ DigitCharacter ..], 
     ToExpression[#], #] &,
   inputText,
   {Depth[inputText] - 1, Depth[inputText]}];

input = toExpression[StringSplit[#, ","] & /@ StringSplit[Import[inputPath], "\n"]];

(* Setup *)

neighbors[list_, {i_, j_}] :=
  Select[
   {i, j} + # & /@ {{-1, 0}, {1, 0}, {0, -1}, {0, 1}},
     1 <= #[[1]] <= Length[list] \[And] 
     1 <= #[[2]] <= Length[list[[i]]] &];

endDistance[n_] :=
 Module[{maze, graph},
  maze = Normal[SparseArray[(input[[;; n]] + 1) -> 1]];
  
  graph =
   Graph[
    Flatten[
     Table[
      ToString[{x, y}] \[DirectedEdge] ToString[#] & /@
       
       Select[neighbors[maze, {x, y}], 
        maze[[#[[1]], #[[2]]]] == maze[[x, y]] == 0 &],
      {x, Length[maze]}, {y, Length[maze[[1]]]}]]];
  GraphDistance[graph, "{1, 1}", "{71, 71}"]

  ]

(* Part 1 *)

endDistance[1024]

(* Part 2 *)

binarySearch[lim_, f_Function] :=
 Module[{low, high, mid},
  low = 1;
  high = lim;
  mid = Round[Mean[{low, high}]];
  
  While[high - low > 2,
   If[f[mid],
    low = mid,
    high = mid];
   mid = Round[Mean[{low, high}]];
   ];
  mid + 1]

StringReplace[
 ToString[input[[
   binarySearch[Length[input], endDistance[#] != \[Infinity] &]]]], 
 " " | "{" | "}" -> ""]