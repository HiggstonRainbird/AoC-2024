(* ::Package:: *)

(* ::Text:: *)
(*Written December 17th, 2024.*)

day = 17;
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
   StringCases[Import[inputPath], DigitCharacter ..]];
registers = input[[;; 3]];
program = input[[4 ;;]];

(* Setup *)

simulateProgram[a_] :=
 Module[{},
  state = <|"A" -> a, "B" -> registers[[2]], "C" -> registers[[3]], 
    "ptr" -> 1|>;
  combo[num_] :=
   Which[
    num <= 3, num,
    num == 4, state["A"],
    num == 5, state["B"],
    num == 6, state["C"]
    ];
  output = {};
  interpretLine[line_List, ptr_] :=
   Module[{}, Which[
     line[[ptr]] == 0, state["A"] = BitShiftRight[state["A"], combo[line[[ptr + 1]]]],
     line[[ptr]] == 1, state["B"] = BitXor[state["B"], line[[ptr + 1]]],
     line[[ptr]] == 2, state["B"] = Mod[combo[line[[ptr + 1]]], 8],
     line[[ptr]] == 3, If[state["A"] != 0, state["ptr"] = line[[ptr + 1]] - 2 + 1],
     line[[ptr]] == 4, state["B"] = BitXor[state["B"], state["C"]],
     line[[ptr]] == 5, AppendTo[output, Mod[combo[line[[ptr + 1]]], 8]],
     line[[ptr]] == 6, state["B"] = BitShiftRight[state["A"], combo[line[[ptr + 1]]]],
     line[[ptr]] == 7, state["C"] = BitShiftRight[state["A"], combo[line[[ptr + 1]]]]
     ];
    state["ptr"] += 2;
    ];
  While[
   state["ptr"] < Length[program],
   interpretLine[program, state["ptr"]];
   ];
  output
  ]

(* Part 1 *)

StringReplace[ToString[simulateProgram[registers[[1]]]], Whitespace | "{" | "}" -> ""]

(* Part 2 *)

possible = {{}};
digits = 1;

While[digits <= Length[program],
  possible = Select[
    Union[Flatten[Table[Join[p, {i}], {i, 0, 7}, {p, possible}], 1]],
    simulateProgram[FromDigits[#, 8]] == program[[-digits ;; -1]] &];
  digits += 1;
  ];
Min[FromDigits[#, 8] & /@ possible]