(* ::Package:: *)

(* ::Text:: *)
(*Written December 7th, 2024.*)

(*Import*)

day = 7;
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
   StringSplit[#, ": " | " "] & /@ 
    StringSplit[Import[inputPath], "\n"]];

(* Part 1 *)

allPossibilities[list_] := Fold[Flatten[{#1 + #2, #1*#2}] &, list];
Select[input, MemberQ[allPossibilities[#[[2 ;;]]], #[[1]]] &][[;; , 1]] // Total

(*Part 2*)

concat[l_List, n2_Integer] := Table[FromDigits[Flatten[Join[IntegerDigits/@{n,n2}]], {n, l}];
nextStep[{n1_, n2_}, lim_] := Select[Union[Flatten[{n1 + n2, n1*n2, concat[n1, n2]}]], # <= lim &];
allPossibilities2[list_, lim_] := Fold[nextStep[{#1, #2}, lim] &, list];
Select[input, MemberQ[allPossibilities2[#[[2 ;;]], #[[1]]], #[[1]]] &][[;;,1]] // Total
