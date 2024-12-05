(* ::Package:: *)

(* ::Text:: *)
(*Written December 4th, 2024.*)

(*Import*)

day = 5;
inputPath = 
  FileNameJoin[{NotebookDirectory[], 
    "Day" <> ToString[day] <> "Input.txt"}];
    
input = ToExpression[
   StringSplit[StringSplit[Import[inputPath], "\n"], 
    Alternatives["|", ","]]];

(* Part 1 *)

ordering = Select[input, Length[#] == 2 &];
pages = Select[input, Length[#] > 2 &];
ClearAll@ordered;
Do[
  ordered[pair[[1]], pair[[2]]] = 1; 
  ordered[pair[[2]], pair[[1]]] = -1, 
  {pair, ordering}];

Total[#[[(Length[#] + 1)/2]] & /@ 
  Select[pages, OrderedQ[#, ordered] &]]

(*Part 2*)

Total[#[[(Length[#] + 1)/2]] & /@ (Sort[#, ordered] & /@ 
    Select[pages, ! OrderedQ[#, ordered] &])]