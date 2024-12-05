(* ::Package:: *)

(* ::Text:: *)
(*Written December 4th, 2024.*)

(*Import*)

day = 4;
inputPath = 
  FileNameJoin[{NotebookDirectory[], 
    "Day" <> ToString[day] <> "Input.txt"}];
    
input = Characters /@ Import[inputPath, "List"];

(* Part 1 *)

Sum[
 Total@StringCount[StringJoin /@ mat, "XMAS" | "SAMX", Overlaps -> True],
 {mat, {input, Transpose[input],
   Table[Diagonal[input, i], {i, -Length@input, Length[input]}],
   Table[Diagonal[Reverse /@ input, i], {i, -Length@input, Length[input]}]}}]

(*Part 2*)

neighborsD[list_, {i_, j_}] := Select[
   {i, j} + # & /@ {{-1, -1}, {-1, 1}, {1, -1}, {1, 1}},
   1 <= #[[1]] <= Length[list] && 1 <= #[[2]] <= Length[list[[i]]] &];
part[mat_, lis_] := 
  If[Depth[lis] == 1, Part[mat, Sequence @@ lis], 
   Table[Part[mat, Sequence @@ l], {l, lis}]];

aPos = Position[input, "A"];
Count[aPos, _?(MemberQ[Characters /@ {"MMSS", "MSMS", "SMSM", "SSMM"},
      part[input, neighborsD[input, #]]] &)]