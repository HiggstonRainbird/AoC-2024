(* ::Package:: *)

(* ::Text:: *)
(*Written December 10th, 2024.*)

(*Import*)

day = 10;
inputPath = 
  FileNameJoin[{NotebookDirectory[], 
    "Day" <> ToString[day] <> "Input.txt"}];

input = toExpression[Characters /@ StringSplit[Import[inputPath], "\n"]];

(* Setup: *)

neighbors[list_, {i_, j_}] := 
  Select[{i, j} + # & /@ {{-1, 0}, {1, 0}, {0, -1}, {0, 1}}, 
    And @@ Thread[1 <= # <= Dimensions[list]] &];
heads = ToString /@ Position[input, 0];
tails = ToString /@ Position[input, 9];
graph = Graph@Flatten@Table[
      ToString[{x, y}] \[DirectedEdge] ToString[#] & /@ 
      Select[neighbors[input, {x, y}], 
        input[[#[[1]], #[[2]]]] == input[[x, y]] + 1 &],
        {x,Length[input]}, {y, Length[input]}];

(* Part 1: *)

Sum[Length[Intersection[VertexOutComponent[graph, h], tails]], {h, heads}]
  
(* Part 2: *)

Total[Table[Length[FindPath[graph, h, #, {9}, Infinity]] & /@ Intersection[VertexOutComponent[graph, h], tails], {h, heads}], 2]