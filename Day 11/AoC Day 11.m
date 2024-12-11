(* ::Package:: *)

(* ::Text:: *)
(*Written December 1th, 2024.*)

(*Import*)

day = 10;
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

input = toExpression[Import[inputPath, "Table"]][[1]];

(* Setup: *)

splitInteger[n_Integer] := {
     FromDigits[#[[1 ;; Length[#]/2]]],
     FromDigits[#[[Length[#]/2 + 1 ;;]]]
     } &@IntegerDigits[n];
rules = {{0, count_Integer} :> {{1, count}},
   {x : _?(EvenQ[Length[IntegerDigits[#]]] &), count_Integer} :> ({#, count} & /@ splitInteger[x]),
   {y : _Integer, count_Integer} :> {{y*2024, count}}};
tallyGather[tallies_List] := {#[[1, 1]], Total[#[[;; , 2]]]} & /@ GatherBy[Flatten[tallies, 1], First];

tally = Tally[input];

(* Part 1: *)

Nest[tallyGather[Replace[#, rules, 1]] &, tally, 25][[;; , 2]] // Total
  
(* Part 2: *)

Nest[tallyGather[Replace[#, rules, 1]] &, tally, 75][[;; , 2]] // Total

(* Part 3 *)

tally = {{125, 1}, {17, 1}};
distinct = 
  Union[Flatten[
    FixedPointList[
      Union[{#[[1]], 1} & /@ tallyGather[Replace[#, rules, 1]]] &, 
      tally][[;; , ;; , 1]]]];
indices = Thread[distinct -> Range[Length[distinct]]];
reverseIndices = Thread[Range[Length[distinct]] -> distinct];
transitions = 
  SparseArray[#[[1]] -> #[[2]] & /@ 
    tallyGather[
     Table[(({d, #[[1]]} /. indices) -> 1) & /@ 
       Replace[{d, 1}, rules], {d, distinct}]]];
start = Table[
   Boole[MemberQ[tally[[;; , 1]] /. indices, i]], {i, 
    Length[indices]}];

Table[
  power = Algebra`MatrixPowerMod[Normal[transitions], p, 10^9];
  {p, Mod[Total[start . power], 10^9]}
  , {p, {1, 25, 75, 10^2, 10^16, 10^100}}] // TableForm