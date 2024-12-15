(* ::Package:: *)

(* ::Text:: *)
(*Written December 15th, 2024.*)

day = 15;
inputPath = 
  FileNameJoin[{NotebookDirectory[], 
    "Day" <> ToString[day] <> "Input.txt"}];

input = StringSplit[Import[inputPath], "\n\n"];

(* Setup*)

moves = Characters@StringReplace[StringJoin[input[[2]]], "\n" -> ""];
dirs = {"^" -> {-1, 0}, ">" -> {0, 1}, "v" -> {1, 0}, "<" -> {0, -1}};

(* Part 1 *)

grid = Characters /@ StringSplit[input[[1]], "\n"];
ClearAll@move;
move[dir_] :=
  Module[{pos, nextPos, newNextPos},
   pos = FirstPosition[grid, "@"];
   nextPos = pos + (dir /. dirs);
   
   While[grid[[nextPos[[1]], nextPos[[2]]]] == "O", 
    nextPos += (dir /. dirs)];
   If[
    grid[[nextPos[[1]], nextPos[[2]]]] == ".",
    grid[[nextPos[[1]], nextPos[[2]]]] = "O";
    grid[[pos[[1]], pos[[2]]]] = ".";
    newNextPos = pos + (dir /. dirs);
    grid[[newNextPos[[1]], newNextPos[[2]]]] = "@";
    ]
   ];
move /@ moves;
Total[{100, 1} . (# - 1) & /@ Position[grid, "O"]]

(* Part 2 *)

grid = Characters /@ 
   StringSplit[
    StringReplace[
     input[[1]], {"#" -> "##", "O" -> "[]", "." -> "..", 
      "@" -> "@."}], "\n"];

ClearAll@move;
move[dir_] :=
  
  Module[{pos, nextPos, sign, posQueue, isBlocked, blocksToMove, 
    vacant, tmpPos, futureBlocks, allBlocks, tmpGrid},
   pos = FirstPosition[grid, "@"];
   nextPos = pos + (dir /. dirs);
   
   If[dir == "<" || dir == ">",
    (* Horizontal Movement *)
    sign = Sign[nextPos[[2]] - pos[[2]]];
    While[MemberQ[{"[", "]"}, grid[[nextPos[[1]], nextPos[[2]]]]], 
     nextPos += (dir /. dirs)];
    If[grid[[nextPos[[1]], nextPos[[2]]]] != "#",
     grid[[pos[[1]], 
       pos[[2]] ;; nextPos[[2]] ;; Sign[nextPos[[2]] - pos[[2]]]]] =
      RotateRight[
       grid[[pos[[1]], pos[[2]] ;; nextPos[[2]] ;; sign]]]];,
    
    (* Vertical Movement *)
    sign = Sign[nextPos[[1]] - pos[[1]]];
    posQueue = CreateDataStructure["Queue", {nextPos}];
    blocksToMove = CreateDataStructure["HashSet", {pos}];
    isBlocked = False;
    While[posQueue["Length"] > 0,
     tmpPos = posQueue["Pop"];
     Which[
      grid[[tmpPos[[1]], tmpPos[[2]]]] == "#",
      isBlocked = True; Break[],
      
      grid[[tmpPos[[1]], tmpPos[[2]]]] == "[", 
      blocksToMove["Insert", {tmpPos[[1]], tmpPos[[2]]}];
      blocksToMove["Insert", {tmpPos[[1]], tmpPos[[2]] + 1}];
      posQueue["Push", {tmpPos[[1]] + sign, tmpPos[[2]]}];
      posQueue["Push", {tmpPos[[1]] + sign, tmpPos[[2]] + 1}];
      ,
      
      grid[[tmpPos[[1]], tmpPos[[2]]]] == "]",
      blocksToMove["Insert", {tmpPos[[1]], tmpPos[[2]]}];
      blocksToMove["Insert", {tmpPos[[1]], tmpPos[[2]] - 1}];
      posQueue["Push", {tmpPos[[1]] + sign, tmpPos[[2]]}];
      posQueue["Push", {tmpPos[[1]] + sign, tmpPos[[2]] - 1}];
      ];
     ];
    If[! isBlocked,
     allBlocks = blocksToMove["Elements"];
     futureBlocks = ({sign, 0} + #) & /@ allBlocks;
     tmpGrid = grid;
     Do[tmpGrid[[futureBlocks[[i, 1]], futureBlocks[[i, 2]]]] = 
       grid[[allBlocks[[i, 1]], allBlocks[[i, 2]]]], {i, 
       Length[allBlocks]}];
     vacant = Complement[allBlocks, futureBlocks];
     Do[tmpGrid[[v[[1]], v[[2]]]] = ".", {v, vacant}];
     grid = tmpGrid;
     ]
    ]
   ];

move /@ moves;
Total[{100, 1} . (# - 1) & /@ Position[grid, "["]]