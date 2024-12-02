(* ::Package:: *)

(* ::Text:: *)
(*Written December 2nd, 2024.*)

(*Import*)

day = 2;
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
input = toExpression[Import[inputPath, "Table"]];

(*Setup*)

safeQ[list_] := (Min[Differences[list]] > 0 \[Or] Max[Differences[list]] < 0) \[And]
    Min[Abs[Differences[list]]] >= 1 \[And] Max[Abs[Differences[list]]] <= 3

(*Part 1*)

Count[input, _?safeQ]

(*Part 2*)

Count[Table[AnyTrue[Table[Delete[line, i], {i, Length[line]}], safeQ], {line, input}], True]
