(* ::Package:: *)

BeginPackage["EigenDiagonal`"];


MF::usage = "same with //MatrixForm";


AmRE::usage = "A-\[Lambda]E";


RamFac::usage = "Foctor for Eigenvalue";


Diagonalized::usage = "Diagonalized matrix";


Begin["`Private`"];


MF[mat_]:=mat//MatrixForm


RamMat:=DiagonalMatrix[{ram, ram, ram}]


AmRE[mat_]:=mat-RamMat


RamFac[mat_]:=Factor[Det[AmRE[mat]]]


Diagonalized[mat_]:=Inverse[Transpose[Eigenvectors[mat]]].mat.Transpose[Eigenvectors[mat]]//Chop


End[];


EndPackage[];
