(* ::Package:: *)

(* ::Text:: *)
(*Written December 8th, 2024.*)

(*Import*)

day = 8;
inputPath = 
  FileNameJoin[{NotebookDirectory[], 
    "Day" <> ToString[day] <> "Input.txt"}];

input = Characters /@ Import[inputPath, "List"];

(* Setup *)

characters = Union[Flatten[input]][[2 ;;]];
lims = Dimensions@input;
dxdy[{{x1_, y1_}, {x2_, y2_}}] := {x1 - x2, y1 - y2};

(* Part 1 *)

Length@Union@
  Flatten[Table[
    Select[Flatten[
      Table[#[[1]] - i*dxdy[#], {i, {2, -1}}] & /@ 
       Subsets[Position[input, c], {2}], 1], 
     And @@ Thread[{0, 0} < # <= lims] &], {c, characters}], 1]

(*Part 2*)

Length@Union@
  Flatten[Table[
    Select[Flatten[
      Table[#[[1]] - i*dxdy[#], {i, -50, 50}] & /@ 
       Subsets[Position[input, c], {2}], 1], 
     And @@ Thread[{0, 0} < # <= lims] &], {c, characters}], 1]