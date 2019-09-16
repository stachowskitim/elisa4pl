(* ::Package:: *)

BeginPackage["elisa4pl`"]

elisa4pl::usage="elisa4pl[x,y,e] calculates a 4-parameter logistic regression from x,y,and error values"

Begin["`Private"]

elisa4pl[xin_,yin_,ein_:0]:=
Module[
{fourpl,fits,err1,err2,plotopts,plots},
fourpl = d+(a-d)/(1+(x/c)^b);
fits = Table[NonlinearModelFit[Thread[{xin,yin[[y]]}],fourpl,{a,b,c,d},x],{y,1,Length[yin]}];
err1 =Table[Around[yin[[x,y]],ein[[x,y]]],{y,1,Length[yin[[1]]]},{x,1,Length[yin]}];
err2=Table[err1[[All,y]],{y,1,Length[yin]}];
plotopts = {FrameStyle->Black,Frame->True,FrameTicksStyle->14,PlotStyle->ColorData[97,y]};
plots=Show[{
Table[
LogLinearPlot[fits[[y]][x],{x,Min[xin],Max[xin]},Evaluate@plotopts]
,{y,1,Length[fits]}],
Table[
ListLogLinearPlot[Thread[{xin,err2[[y]]}],Evaluate@plotopts,PlotRange->{{Min[xin],Max[xin]},{Min[yin],(Max[yin]+Max[ein])}}]
,{y,1,Length[fits]}],
Table[
ListLogLinearPlot[Thread[{xin,yin[[y]]}],Evaluate@plotopts,PlotRange->{{Min[xin],Max[xin]},{Min[yin],(Max[yin]+Max[ein])}}]
,{y,1,Length[fits]}]
}];
Print@plots;
Print@Table[{ToString[y],fits[[y]]["BestFitParameters"]},{y,1,Length[fits]}];
Export["elisa.pdf",plots];
]

End[]

EndPackage[]



