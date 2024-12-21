(* ::Package:: *)

(* ::Text:: *)
(*Written December 21st, 2024.*)

day = 21;
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

input = toExpression[
   Characters /@ StringSplit[Import[inputPath], "\n"]];

(* Setup *)

directionKeys = {">", "<", "^", "v", "A"};
directionPaths = {
   {"^" \[DirectedEdge] "A", "<" \[DirectedEdge] "v", 
    "v" \[DirectedEdge] ">"},
   {"A" \[DirectedEdge] "^", "v" \[DirectedEdge] "<", 
    ">" \[DirectedEdge] "v"},
   {"v" \[DirectedEdge] "^", ">" \[DirectedEdge] "A"},
   {"^" \[DirectedEdge] "v", "A" \[DirectedEdge] ">"}
   };
directionals = 
  Table[{#[[3, 1]], #[[1]]} \[DirectedEdge] {#[[3, 2]], #[[2]]} & /@ 
    Tuples[{directionKeys, directionKeys, path}], {path, 
    directionPaths}];

numberPaths = {
   {7 \[DirectedEdge] 8, 8 \[DirectedEdge] 9, 4 \[DirectedEdge] 5, 
    5 \[DirectedEdge] 6, 1 \[DirectedEdge] 2, 2 \[DirectedEdge] 3, 
    0 \[DirectedEdge] "A"},
   {8 \[DirectedEdge] 7, 9 \[DirectedEdge] 8, 5 \[DirectedEdge] 4, 
    6 \[DirectedEdge] 5, 2 \[DirectedEdge] 1, 3 \[DirectedEdge] 2, 
    "A" \[DirectedEdge] 0},
   {1 \[DirectedEdge] 4, 4 \[DirectedEdge] 7, 0 \[DirectedEdge] 2, 
    2 \[DirectedEdge] 5, 5 \[DirectedEdge] 8, "A" \[DirectedEdge] 3, 
    3 \[DirectedEdge] 6, 6 \[DirectedEdge] 9},
   {4 \[DirectedEdge] 1, 7 \[DirectedEdge] 4, 2 \[DirectedEdge] 0, 
    5 \[DirectedEdge] 2, 8 \[DirectedEdge] 5, 3 \[DirectedEdge] "A", 
    6 \[DirectedEdge] 3, 9 \[DirectedEdge] 6}
   };
numericals = 
  Table[{#[[3, 1]], #[[1]]} \[DirectedEdge] {#[[3, 2]], #[[2]]} & /@ 
    Tuples[{directionKeys, directionKeys, path}], {path, numberPaths}];

getWeights[sourceGraph_, edge_, key_] :=
  If[Length[key] == 0,
   GraphDistance[sourceGraph, edge[[1, 2]], key] + 
    GraphDistance[sourceGraph, key, edge[[2, 2]]] + 1,
   GraphDistance[sourceGraph, {edge[[1, 2]], "A"}, key] + 
    GraphDistance[sourceGraph, key, {edge[[2, 2]], "A"}] + 1
   ];
nextKeypad[sourceGraph_, depth_Integer, edges_List] :=
  
  Module[{weights},
   weights =
    Table[
     # -> getWeights[
         sourceGraph,
         #,
         If[depth == 1, 
          directionKeys[[dir]], {directionKeys[[dir]], "A"}]
         ] & /@ edges[[dir]],
     {dir, 4}];
   Graph[Flatten[edges], EdgeWeight -> Flatten[weights]]];
finalKeypad[depth_Integer] :=
  Module[{keypad},
   keypad = 
    Graph[Flatten[directionPaths], VertexLabels -> Automatic];
   Do[
    keypad = nextKeypad[keypad, i, directionals],
    {i, depth - 1}];
   keypad = nextKeypad[keypad, depth, numericals]];

(* Part 1 *)

pad = finalKeypad[2];
Round[
 Sum[
  FromDigits[Select[code, IntegerQ]]*
   Total[
    GraphDistance[pad, {#[[1]], "A"}, {#[[2]], "A"}] + 1 & /@
     
     Partition[Join[{"A"}, code], 2, 1]],
  {code, input}]]

(* Part 2 *)

pad = finalKeypad[25];
Round[
 Sum[
  FromDigits[Select[code, IntegerQ]]*
   Total[
    GraphDistance[pad, {#[[1]], "A"}, {#[[2]], "A"}] + 1 & /@
     
     Partition[Join[{"A"}, code], 2, 1]],
  {code, input}]]