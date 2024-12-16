(* ::Package:: *)

(* ::Text:: *)
(*Written December 16th, 2024.*)

day = 16;
inputPath = 
  FileNameJoin[{NotebookDirectory[], 
    "Day" <> ToString[day] <> "Input.txt"}];

input = StringSplit[Import[inputPath], "\n\n"];

(* Setup *)

dirs = {{0, 1}, {-1, 0}, {1, 0}, {0, -1}};
neighbors[list_, {i_, j_}] :=
  Select[
   {i, j} + # & /@ dirs,
   1 <= #[[1]] <= Length[list] \[And] 
     1 <= #[[2]] <= Length[list[[i]]] \[And] 
     list[[#[[1]], #[[2]]]] != "#" &];
nextMove[{{pos_, dir_}, 
    cost_}] :=
  {{#, FirstPosition[dirs, # - pos][[1]]},
     cost + 
      Which[pos + dirs[[dir]] == #, 1, pos - dirs[[dir]] != #, 1001, 
       True, Infinity]
     } & /@ neighbors[input, pos];

start = FirstPosition[input, "S"];
goal = FirstPosition[input, "E"];

(* Part 1 *)

ClearAll@best;
best[{pos_, dir_}] := Infinity;
best[{start, 1}] = 0;
conf = {{{start, 1}, 0}};

While[
  Length[conf] > 0,
  
  conf = Select[
    Union[Flatten[Table[n, {c, conf}, {n, nextMove[c]}], 1]], 
    best[#[[1]]] > #[[2]] &];
  conf = {#[[1, 1]], Min[#[[;; , 2]]]} & /@ GatherBy[conf, First];
  Do[best[c[[1]]] = Min[best[c[[1]]], c[[2]]], {c, conf}]
  ];
Min[Table[best[{goal, n}], {n, 4}]]

(* Part 2 *)

curr = Table[{FirstPosition[input, "E"], m}, {m, 
    MinimalBy[Range[4], best[{FirstPosition[input, "E"], #}] &]}];
allPaths = curr[[;; , 1]];
moveCost[{posA_, dirA_}, {posB_, dirB_}] := If[dirA == dirB, 1, 1001]
While[Length[curr] > 0,
  allPaths = Join[allPaths, curr[[;; , 1]]];
  curr = Union@Flatten[Table[
      Select[
       Flatten[Table[{#, d} & /@ neighbors[input, c[[1]]], {d, 1, 4}],
         1],
       best[#] + moveCost[c, #] == best[c] &],
      {c, curr}], 1]];
allPaths = Union[allPaths];
Length[allPaths]