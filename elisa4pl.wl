(* ::Package:: *)

(* This package can be loaded into a Mathematica notebook with Needs["elisa4pl`"] 
and is run on a data series of known concentrations and intensity values with elisa4pl[xyzdata] and returns a plot of the fit to the data and table of fitting parameters *)

BeginPackage["elisa4pl`"]
elisa4pl::usage="elisa4pl[x,y,e] calculates a 4-parameter logistic regression on x,y, and error values"
Begin["Private`"]

(* input is three separate lists corresponding to concentrations (x), intensity values (y), and errors in intensity (e, default to 0s if not provided) *)
elisa4pl[xin_,yin_,ein_:0]:=

Module[
{fourpl,fits,err1,err2,plotopts,plots,colors},

(* four parameter logistic regression, 4pl, equation where:
	a = theoretical response at concentration=0
	b = slope factor (Hill's factor)
	c = mid-range concentration
	d = theoretical response at concentration=infinity
EC50/IC50 is the concentration, x where the intensity, y = 50% *)
fourpl = d+(a-d)/(1+(x/c)^b);

(* fit data to equation *)
fits = Table[NonlinearModelFit[Thread[{xin,yin[[y]]}],fourpl,{a,b,c,d},x,MaxIterations -> \[Infinity]],{y,1,Length[yin]}];

(* associate intensities and corresponding errors *)
err1 =Table[Around[yin[[x,y]],ein[[x,y]]],{y,1,Length[yin[[1]]]},{x,1,Length[yin]}];
err2=Table[err1[[All,y]],{y,1,Length[yin]}];

(* prints visualization of fit to the data *)
plotopts = {PlotRange->All,FrameStyle->Black,Frame->True,FrameTicksStyle->14,PlotStyle->ColorData[97,y]};
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
colors = Table[ColorData[97,y],{y,1,Length[yin]}];
Print@plots;

(* prints parameter table *)
Print@Table[{ToString[y],colors[[y]],fits[[y]]["ParameterTable"]},{y,1,Length[fits]}];

(* saves pdf of fit to current working directory*)
Export["elisa.pdf",plots];
]

End[]
EndPackage[]









