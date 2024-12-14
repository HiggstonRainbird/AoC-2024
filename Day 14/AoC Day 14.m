(* ::Package:: *)

(* ::Text:: *)
(*Written December 14th, 2024.*)

day = 14;
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
    StringSplit[#, "," | "=" | " "] & /@ 
     StringSplit[Import[inputPath], "\n"]][[;; , {2, 3, 5, 6}]];

(* Setup*)

inputPairs={#[[{1,2}]],#[[{3,4}]]}&/@input;
dims={101,103};
nextStep[{p_,v_},n_:1]:={Mod[p+n*v,dims],v};

(* Part 1 *)

future=nextStep[#,100]&/@inputPairs;
quadrant[p_]:=Total[2^{1,0} Boole[Thread[p<(dims-1)/2]]];
Times@@(Length/@GatherBy[Select[future,#[[1,1]]!=(dims[[1]]-1)/2\[And]#[[1,2]]!=(dims[[2]]-1)/2&],quadrant[#[[1]]]&])

(* Part 2 *)

inputPairs={#[[{1,2}]],#[[{3,4}]]}&/@input;
NestWhile[{#[[1]]+1,nextStep/@#[[2]]}&,{0,inputPairs},Max[Length/@ConnectedComponents[NearestNeighborGraph[#[[2,;;,1]]]]]<100&][[1]]