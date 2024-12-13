(* ::Package:: *)

(* ::Text:: *)
(*Written December 13th, 2024.*)

(*Import*)

day = 13;
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
   StringSplit[#, ", " | ": " | "+" | "="] & /@ 
    StringSplit[Import[inputPath], "\n"]];

(* Setup: *)

rules = <|"A" -> #[[1, {3, 5}]], "B" -> #[[2, {3, 5}]], "P" -> #[[3, {3, 5}]]|> & 
  /@ Partition[input, 4, 4, {1, -2}, {}];

(* Part 1: *)

Total[3 a + b /. #[[1]] & /@ Select[
   Table[
    Solve[r["A"]*a + r["B"]*b == r["P"] \[And] 100 >= {a, b} >= 0, 
     Integers],
    {r, rules}],
   # =!= {} &]]
  
(* Part 2: *)

Total[3 a + b /. #[[1]] & /@ Select[
   Table[
    Solve[r["A"]*a + r["B"]*b == r["P"] + 10^13 \[And] {a, b} >= 0, 
     Integers],
    {r, rules}],
   # =!= {} &]]