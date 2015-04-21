(* Wolfram Language Package *)

(* Created by the Wolfram Workbench Apr, 2015 
   Author: Paco Jain (pacojain@gmail.com)
*)

BeginPackage["VisualDebug`"]
(* Exported symbols added here with SymbolName::usage *)

VisualDebug::usage= "Main utility"

Begin["`Private`"]
(* Implementation of the package *)

SetAttributes[VisualDebug, HoldAll]



SetAttributes[shape, HoldAll]
shape[expr_] := Module[
  {
   
   },
  If[Depth[Unevaluated[expr]] == 1,
   Return[0],
   Return[{Length[Unevaluated[expr]], 
     shape @@@ (Map[Unevaluated, 
         ReplacePart[Hold[expr], {1, 0} -> List], {2}] // 
        ReleaseHold)}]
   ]
  ]

End[]

EndPackage[]