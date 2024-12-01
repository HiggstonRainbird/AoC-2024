(* ::Package:: *)

(* ::Text:: *)
(*Written December 1st, 2024.*)

(*Import*)

inputPath = 
  FileNameJoin[{NotebookDirectory[], 
    "Day" <> ToString[day] <> "Input.txt"}];
input = toExpression[Import[inputPath, "Table"]];

(*Setup*)

{left, right} = Transpose[input];

(*Part 1*)

Total[ManhattanDistance @@@ Transpose[{Sort[left], Sort[right]}]]

(*Part 2*)

Total[#*Count[right, #] & /@ left]
