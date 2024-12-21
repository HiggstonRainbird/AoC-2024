(* ::Package:: *)

(* ::Text:: *)
(*Written December 19th, 2024.*)

day = 19;
inputPath = 
  FileNameJoin[{NotebookDirectory[], 
    "Day" <> ToString[day] <> "Input.txt"}];

input = StringSplit[Import[inputPath], "\n\n"];
input = StringSplit[#, "\n"] & /@ input;

(* Setup *)

{patterns, goals} = input;
patterns = StringSplit[patterns, ", "][[1]];

ClearAll@count;
count[""] := 1;
count[goal_] := 
  count[goal] = 
   Total[count[StringTake[goal, {StringLength[#] + 1, -1}]] & /@ 
     Select[patterns, StringStartsQ[goal, #] &]];

(* Part 1 *)

Count[goals, _?(count[#] != 0 &)]

(* Part 2 *)

Total[count /@ goals]