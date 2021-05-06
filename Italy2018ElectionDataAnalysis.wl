(* ::Package:: *)

(* :Title : Italy 2018 Election Data Analysis *)
(* :Context : Italy 2018 Election Data Analysis *)
(* :Author : ConteTer (Marco Ferrati, Michele Perlino, Tommaso Azzalin, Vittoria Conte) *)
(* :Summary: Package for analysing the 2018 Italian political election data published by the the Ministry of the Interior. *)
(* :Copyright : MIT *)
(* :Package Version : 1 *)
(* :Mathematica Version : 12.2*)
(* :History : *)
(* :Keywords : italy, election, 2018, data, analysis*)
(* :Sources : https://github.com/jjocram/italy-2018-election-data-mathematica *)
(* :Discussion : *)

PlottingElectionEloctorsPie::usage = "PlottingElectionEloctorsPie[chamber, region: Null, province: Null, district: Null, query: Null] returns a list of data to plot the electors pie chart."
PlottingElectionVotersPie::usage = "PlottingElectionVotersPie[chamber, region: Null, province: Null, district: Null, query: Null] returns a list of data to plot the voters pie chart."
PlottingElectionVotersNonVotersPie::usage = "PlottingElectionVotersNonVotersPie[chamber, region: Null, province: Null, district: Null, query: Null] returns a list of data to plot the voters and non voters pie chart."
PlottingElectionRegionCoalitionsBars::usage = "PlottingElectionRegionCoalitionsBars[chamber, coalition, region: Null, province: Null, district: Null, query: Null] returns a list of data to plot in each region the winning coalition."
PlottingCandidate::usage = "PlottingCandidate[name, surname, city: Null] returns a list of data to plot the histogram for the candidate."

Begin["Private`"]
	PlottingElectionEloctorsPie[_chamber, _region: Null, _province: Null, _district: Null, _query: Null] := 
		Return []
		
	PlottingElectionVotersPie[_chamber, _region: Null, _province: Null, _district: Null, _query: Null] :=
		Return []
		
	PlottingElectionVotersNonVotersPie[_chamber, _region: Null, _province: Null, _district: Null, _query: Null] :=
		Return []
		
	PlottingElectionRegionCoalitionsBars[_chamber, _coalition, _region: Null, _province: Null, _district: Null, _query: Null] :=
		Return []

	PlottingCandidate[_name, _surname, _city: Null] :=
		Return []
End[]
