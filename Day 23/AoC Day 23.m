(* ::Package:: *)

(* ::Text:: *)
(*Written December 23rd, 2024.*)

day = 23;
inputPath = 
  FileNameJoin[{NotebookDirectory[], 
    "Day" <> ToString[day] <> "Input.txt"}];

input = StringSplit[#, "-"] & /@ StringSplit[Import[inputPath], "\n"];

(* Setup *)

g = Graph[#[[1]] \[UndirectedEdge] #[[2]] & /@ input];

(* Part 1 *)

(* Original*) 
vertices = VertexList[g];
tVertices = Select[vertices, StringMatchQ[#, "t*"] &];
findCompleteClusters[node_] := 
Sort[Join[{node}, #]] & /@
  Select[Subsets[VertexOutComponent[g, node, {1}], {2}], 
  GraphDistance[g, #[[1]], #[[2]]] == 1 &];
Union[Flatten[findCompleteClusters /@ tVertices, 1]] // Length

(* FindCycle[] - From Reddit *)
Length@Select[FindCycle[g, {3}, Infinity], 
   IntersectingQ[tVertices, VertexList[#]] &];
Length[Union[
   Flatten[Table[
     Sort /@ FindCycle[{g, t}, {3}, Infinity][[;; , ;; , 1]], {t, 
      tVertices}], 1]]];

(* FindClique[] - From Tim *)
Length@Union[
   Sort /@ Flatten[
     Select[Subsets[#, {3}], IntersectingQ[#, tVertices] &] & /@ 
      Select[Flatten[FindClique[{g, #}, Infinity, All] & /@ tVertices,
         1], Length[#] > 2 &], 1]] // AbsoluteTiming

(* Part 2 *)

StringJoin[Riffle[Sort[FindClique[g][[1]]], ","]]