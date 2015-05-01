(* ::Package:: *)

(* Wolfram Language Package *)

(* Created by the Wolfram Workbench Apr, 2015 
   Author: Paco Jain (pacojain@gmail.com)
*)

BeginPackage["VisualDebug`"]
(* Exported symbols added here with SymbolName::usage *)

VisualDebug::usage= "Main utility"
step
stepTrace
stepTracePrint
debug
shape

Begin["`Private`"]
(* Implementation of the package *)

SetAttributes[stepTracePrint, HoldFirst]
stepTracePrint[expr_, opts: OptionsPattern[{"OutputWrapper" -> Defer, "HaltingContexts" -> {"Global`"}, "MaxSteps"-> 100}]] := Module[
	{
		stepTraceList = stepTrace[expr, opts]
	},
	Scan[Print, stepTraceList]
]

SetAttributes[stepTrace, HoldFirst]
stepTrace[expr_, OptionsPattern[{"OutputWrapper" -> Defer, "HaltingContexts" -> {"Global`"}, "MaxSteps"-> 100}]] := Module[
	{
		currentExpr, newExpr = HoldForm[expr], stepNumber = 0
	},
	While[stepNumber < OptionValue["MaxSteps"] && ! MatchQ[newExpr, currentExpr],
		stepNumber++;
		currentExpr = newExpr;
		newExpr = step @@ currentExpr;
		Sow[currentExpr]
	] // Reap // Part[#, 2, 1]&
]

SetAttributes[step, HoldFirst]
step[expr_, OptionsPattern[{"OutputWrapper" -> HoldForm, "HaltingContexts" -> {"Global`"}}]] := Module[
	{
		trace, tracePart, contexts, replacementTarget
	},
	trace = Trace[Unevaluated[expr]];
	If[trace === {}, Return[expr // OptionValue["OutputWrapper"]]];
	contexts[ex_] := Context /@ Cases[ex, s_Symbol -> HoldPattern[s], {0, Infinity}, Heads -> True] // DeleteDuplicates;
	tracePart = ReplaceRepeated[trace, List[first_List, rest___] -> first];
	replacementTarget = Select[
		tracePart // Rest,
		(Intersection[contexts[#], OptionValue["HaltingContexts"]] != {} && Complement[contexts[#], OptionValue["HaltingContexts"]~Union~{"System`"}] === {} && # =!= First[tracePart])&
	] // If[# == {}, Last[tracePart], First[#]] &;
	OptionValue["OutputWrapper"][expr] /. (First[tracePart] /. HoldForm -> HoldPattern) :> RuleCondition @ Extract[replacementTarget, 1, $ConditionHold]
]

SetAttributes[debug, HoldFirst]
debug[origExpr_, opts : OptionsPattern[{"Heads" -> False}]] := Module[
	{
		currentExpr, allowedParts, part
	},
	currentExpr = origExpr;
	allowedParts = Position[currentExpr, _, Heads -> OptionValue["Heads"]] // Sort // Rest;
	Manipulate[
		part = allowedParts[[partNumber]];
		ReplacePart[currentExpr, 
		part -> Style[Extract[currentExpr, part, HoldForm], FontColor -> RGBColor[1, 0, 0], Bold]],
		{partNumber, If[OptionValue["Heads"], 2, 1], Dynamic[Length[allowedParts]], 1},
		Row[{
			Button["Evaluate",	
				currentExpr = ReplacePart[currentExpr, part -> Evaluate[Extract[currentExpr, part]]];
				allowedParts = Position[currentExpr, _, Heads -> OptionValue["Heads"]] // Sort // Rest
			],
			Button["Reset",
				partNumber = If[OptionValue["Heads"], 2, 1];
				currentExpr = origExpr;
				allowedParts = Position[currentExpr, _, Heads -> OptionValue["Heads"]] // Sort // Rest
			]
		}]
	]
]

SetAttributes[shape, HoldAll]
shape[expr_] := Module[
	{ },
	If[Depth[Unevaluated[expr]] == 1,
		(* then *)
		Return[0],
		(* else *)
		Return[{Length[Unevaluated[expr]], shape @@@ (Map[Unevaluated, ReplacePart[Hold[expr], {1, 0} -> List], {2}] // ReleaseHold)}]
	]
]

End[]

EndPackage[]
