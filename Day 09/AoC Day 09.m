(* ::Package:: *)

(* ::Text:: *)
(*Written December 9th, 2024.*)

(*Import*)

day = 9;
inputPath = 
  FileNameJoin[{NotebookDirectory[], 
    "Day" <> ToString[day] <> "Input.txt"}];

input = Import[inputPath, "List"];

(* Part 1 *)

blocks = Partition[IntegerDigits[input[[1]]], 2];
If[OddQ[Length[IntegerDigits[input[[1]]]]], 
  AppendTo[blocks, {IntegerDigits[input[[1]]][[-1]], 0}]];
memory = Flatten[Table[
    Join[Table[j - 1, {i, blocks[[j, 1]]}], 
     Table[Null, {i, blocks[[j, 2]]}]],
    {j, 1, Length[blocks]}
    ]];
firstNull = FirstPosition[memory, Null][[1]];
lastInt = Position[memory, _Integer][[-1, 1]];

While[firstNull < lastInt,
  memory[[firstNull]] = memory[[lastInt]];
  memory[[lastInt]] = Null;
  firstNull += 1; lastInt -= 1;
  
  While[IntegerQ[memory[[firstNull]]], firstNull += 1];
  While[! IntegerQ[memory[[lastInt]]], lastInt -= 1];
  ];
Sum[If[memory[[i]] === Null, 0, memory[[i]]*(i - 1)], {i, 
  Length[memory]}]

(*Part 2*)

blocks = Partition[IntegerDigits[input[[1]]], 2];
If[OddQ[Length[IntegerDigits[input[[1]]]]], 
  AppendTo[blocks, {IntegerDigits[input[[1]]][[-1]], 0}]];
memory = Flatten[Table[
    Join[Table[j - 1, {i, blocks[[j, 1]]}], 
     Table[Null, {i, blocks[[j, 2]]}]],
    {j, 1, Length[blocks]}
    ]];
nullSpans = Split[Position[memory, Null][[;; , 1]], #2 - #1 == 1 &];
filledSpans = 
  Split[Position[memory, _Integer][[;; , 
    1]], #2 - #1 == 1 \[And] memory[[#2]] == memory[[#1]] &];

Do[
  globalWatch = lastSpan;
  spanLength = Length[filledSpans[[lastSpan]]];
  toInsert = 
   FirstPosition[nullSpans, _?(Length[#] >= spanLength &), {0}, {1}, 
     Heads -> False][[1]];
  If[toInsert != 0,
   If[nullSpans[[toInsert, 1]] > filledSpans[[lastSpan, 1]], 
    Continue[]];
   memory[[
     nullSpans[[toInsert, ;; Length[filledSpans[[lastSpan]]]]]]] = 
    memory[[filledSpans[[lastSpan]]]];
   memory[[filledSpans[[lastSpan]]]] = Null;
   nullSpans[[toInsert]] = 
    Delete[nullSpans[[toInsert]], {#} & /@ Range[spanLength]];
   ];,
  {lastSpan, Length[filledSpans], 1, -1}];
Sum[If[memory[[i]] === Null, 0, memory[[i]]*(i - 1)], {i, 
  Length[memory]}]