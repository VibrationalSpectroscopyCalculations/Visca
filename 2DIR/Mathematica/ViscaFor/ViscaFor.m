(* Wolfram Language Package *)

(* Created by the Wolfram Workbench 12.10.2022 *)

BeginPackage["ViscaFor`", {"BRP`", "ImportDCD`"}]
(* Exported symbols added here with SymbolName::usage *) 

$ViscaForDirectory


SIGN::usage = "SIGN[x, y] emulates the Fortran SIGN function."

$DataDir::usage = "$DataDir  " ;

Begin["`Private`"]
(* Implementation of the package *)


$ViscaForDirectory = DirectoryName @ $InputFileName;
$DataDir := FileNameJoin[{$ViscaForDirectory, "data"}];

ViscaFor`$START = AbsoluteTime[];

End[]

EndPackage[]

Get @ FileNameJoin[{ $ViscaForDirectory, "WL", "amide.wl"}]

