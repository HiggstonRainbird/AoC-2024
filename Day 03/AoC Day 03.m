(* ::Package:: *)

(* ::Text:: *)
(*Written December 3rd, 2024.*)

(*Import*)

day = 3;
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

(* Part 1 *)

allMul = StringCases[input, 
   "mul(" ~~ x : DigitCharacter .. ~~ "," ~~ y : DigitCharacter .. ~~ ")" :> 
   toExpression[{x, y}]];
Total[Times @@@ allMul]

(*Part 2*)

mulPos = StringPosition[input, 
   "mul(" ~~ DigitCharacter .. ~~ "," ~~ DigitCharacter .. ~~ ")"];
doPos = Join[{{0, 0}}, StringPosition[input, "do()"]];
dontPos = Join[{{-1, -1}}, StringPosition[input, "don't()"]];
Sum[
 If[Select[doPos[[;; , -1]], # < mulPos[[i, 1]] &][[-1]] >
   Select[dontPos[[;; , -1]], # < mulPos[[i, 1]] &][[-1]], 
  Times @@ allMul[[i]], 0],
 {i, Length[mulPos]}]