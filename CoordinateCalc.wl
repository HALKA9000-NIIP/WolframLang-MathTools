(* ::Package:: *)

BeginPackage["CoordinateCalc`"];


VSSub::usage = "Scalar distance between coordinates";


NSSub::usage = "Numeral, Scalar distance between coordinates";


VSub::usage = "Vector distance between coordinates";


NVSub::usage = "Numeral, Vector distance between coordinates";


NVCent::usage = "Numeral, Center coordinates between coordinates";


VCent::usage = "Center coordinates between coordinates";


NVCent::usage = "Numeral, Center coordinates between coordinates";


Begin["`Private`"];


dpow[exp_]:=exp^2


Mr[vec_]:=N[Sqrt[Total[Map[dpow,vec]]]]


NMr[vec_]:=N[Mr[vec]]


SSub[veca_,vecb_]:=Mr[VSub[veca,vecb]]


NSSub[veca_,vecb_]:=N[SSub[veca,vecb]]


VSub[veca_,vecb_]:=veca-vecb


NVSub[veca_,vecb_]:= N[VSub[veca,vecb]]


VCent[veca_,vecb_]:=(veca+vecb)/2


NVCent[veca_,vecb_]:= N[VCent[veca,vecb]]


End[];


EndPackage[];
