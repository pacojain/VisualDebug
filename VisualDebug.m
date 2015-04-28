(* ::Package:: *)

(* Wolfram Language Package *)

(* Created by the Wolfram Workbench Apr, 2015 
   Author: Paco Jain (pacojain@gmail.com)
*)

BeginPackage["VisualDebug`"]
(* Exported symbols added here with SymbolName::usage *)

VisualDebug::usage= "Main utility"
step
shape

Begin["`Private`"]
(* Implementation of the package *)

SetAttributes[VisualDebug, HoldAll]

SetAttributes[step, HoldFirst]
(*ClearAttributes[step,HoldFirst]*)
step[expr_, OptionsPattern[{"OutputWrapper" -> Defer, "HaltingContexts" -> {"Global`"}}]] := Module[
	{
		trace, tracePart, contexts, replacementTarget
	},
	trace = Trace[expr];
	If[trace === {}, Return[expr]];
	contexts[ex_] := Context /@ Cases[ex, s_Symbol -> HoldPattern[s], {0, Infinity}, Heads -> True] // DeleteDuplicates;
	tracePart = ReplaceRepeated[trace, List[first_List, rest___] -> first];
	replacementTarget = Select[tracePart // Rest, Intersection[contexts[#], OptionValue["HaltingContexts"]] != {} &] // If[# == {}, Last[tracePart], First[#]] &;
	OptionValue["OutputWrapper"][expr] /. (First[tracePart] /. HoldForm -> HoldPattern) -> replacementTarget
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
