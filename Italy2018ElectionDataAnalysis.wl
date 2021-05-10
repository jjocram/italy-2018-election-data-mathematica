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
BeginPackage["Italy2018ElectionDataAnalysis`"]

PlottingElectionElectorsPie::usage = "PlottingElectionElectorsPie[chamber, region: Null, province: Null, district: Null, query: Null] returns a list of data to plot the electors pie chart."
PlottingElectionVotersPie::usage = "PlottingElectionVotersPie[chamber, region: Null, province: Null, district: Null, query: Null] returns a list of data to plot the voters pie chart."
PlottingElectionVotersNonVotersPie::usage = "PlottingElectionVotersNonVotersPie[chamber, region: Null, province: Null, district: Null, query: Null] returns a list of data to plot the voters and non voters pie chart."
PlottingElectionRegionCoalitionsBars::usage = "PlottingElectionRegionCoalitionsBars[chamber, coalition, region: Null, province: Null, district: Null, query: Null] returns a list of data to plot in each region the winning coalition."
PlottingCandidate::usage = "PlottingCandidate[name, surname, city: Null] returns a list of data to plot the histogram for the candidate."
GetChamber::usage = "DEV TOOL GetChamber[] return the chamber dataset"
GetRegions::usage = "GetRegions[] return the list of regions used in this package"

Begin["`Private`"]
	InitDataset[url_] :=
		Module[{file, stringReplaced},
			file = URLDownload[url];
			stringReplaced = StringReplace[ReadString[file], ";"->","];
			Return[ImportString[stringReplaced, "Dataset", HeaderLines->1]];
		]
	
	chamberDataset = InitDataset["https://dait.interno.gov.it/documenti/camera_2018_scrutini_italia.csv"];
	senateDataset = InitDataset["https://dait.interno.gov.it/documenti/senato_2018_scrutini_italia.csv"];
	coalitionsChamber = Association[
		"Destra" -> {"BLOCCO NAZIONALE PER LE LIBERTA'", "CASAPOUND", "FORZA ITALIA", "FRATELLI D'ITALIA CON GIORGIA MELONI", "GRANDE NORD", "IL POPOLO DELLA FAMIGLIA", "ITALIA AGLI ITALIANI", "ITALIA NEL CUORE", "LEGA", "LISTA DEL POPOLO PER LA COSTITUZIONE", "NOI CON L'ITALIA - UDC", "RINASCIMENTO MIR"},
		"Centro" -> {"MOVIMENTO 5 STELLE", "PARTITO REPUBBLICANO ITALIANO - ALA"},
		"Sinistra" -> {"+EUROPA", "10 VOLTE MEGLIO", "CIVICA POPOLARE LORENZIN", "ITALIA EUROPA INSIEME", "LIBERI E UGUALI", "PARTITO DEMOCRATICO", "PARTITO VALORE UMANO", "PER UNA SINISTRA REVOLUZIONARIA", "POTERE AL POPOLO!", "PATTO PER L'AUTONOMIA", "SIAMO"}
	];
	coalitionsSenate = Association[
		"Destra" -> {"BLOCCO NAZIONALE PER LE LIBERTA'", "CASAPOUND ITALIA", "FORZA ITALIA", "FRATELLI D'ITALIA CON GIORGIA MELONI", "GRANDE NORD", "IL POPOLO DELLA FAMIGLIA", "ITALIA AGLI ITALIANI", "ITALIA NEL CUORE", "LEGA", "LISTA DEL POPOLO PER LA COSTITUZIONE", "NOI CON L'ITALIA - UDC", "RINASCIMENTO MIR", "DESTRE UNITE - FORCONI"},
		"Centro" -> {"MOVIMENTO 5 STELLE", "PARTITO REPUBBLICANO ITALIANO - ALA", "SVP - PATT"},
		"Sinistra" -> {"+EUROPA", "10 VOLTE MEGLIO", "CIVICA POPOLARE LORENZIN", "ITALIA EUROPA INSIEME", "LIBERI E UGUALI", "PARTITO DEMOCRATICO", "PARTITO VALORE UMANO", "PER UNA SINISTRA REVOLUZIONARIA", "POTERE AL POPOLO!", "PATTO PER L'AUTONOMIA", "SIAMO", "PARTITO COMUNISTA"}
	];
	regions = {"ABRUZZO", "BASILICATA", "CALABRIA", "CAMPANIA", "EMILIA-ROMAGNA", "FRIULI-VENEZIA GIULIA", "LAZIO", "LIGURIA", "LOMBARDIA", "MARCHE", "MOLISE", "PIEMONTE", "PUGLIA", "SARDEGNA", "SICILIA", "TOSCANA", "TRENITNO-ALTO ADIGE", "UMBRIA", "VALLE D'AOSTA", "VENETO"};
	
	GetRegions[] := regions
	
	GetChamber[] := chamberDataset
	
	(* d is used instead of district because it does not work otherwise *)
	GetRegionFromDistrict[d_] := StringJoin[If[MatchQ[Characters[d],{__, " ", _}], Take[Characters[d], Length[Characters[d]]-2], d]]
	
	(* Filters the given dataset by the given region *)
	FilterRegion[dataset_, region_] := If[region === Null, dataset, dataset[Select[#CIRCOSCRIZIONE == region &]]]
	
	(* Filters the given dataset by the given province *)
	FilterProvince[dataset_, province_] := If[province === Null, dataset, dataset[Select[#PROVINCIA == province &]]]
	
	(* Filters the given dataset by the given district *)
	FilterDistrict[dataset_, district_] := If[district === Null, dataset, dataset[Select[#CIRCOSCRIZIONE == district &]]]
	
	(* Filters the given dataset by the given query typed by the user *)
	FilterQuery[dataset_, query_] :=
		Module [{queries, queryCharacters, qReplaced, params, datasetToReturn},
			datasetToReturn = If[query === Null, Return[dataset], dataset]; (*Necessary to return the right values*)
			queries = StringSplit[query, ","];
			Do[
				qReplaced = StringReplace[q, " "->""];
				queryCharacters = Characters[qReplaced];
				datasetToReturn = If[
					MatchQ[queryCharacters, {__, "<" , __}], 
					(params = StringSplit[qReplaced, "<"]; (* In 1 there is the attribute, in 2 there is the value*)
					datasetToReturn[Select[#[params[[1]]] < ToExpression[params[[2]]] &]]
					), 
					datasetToReturn];
				datasetToReturn = If[
					MatchQ[queryCharacters, {__, "=" , __}], 
					(params = StringSplit[qReplaced, "="]; (* In 1 there is the attribute, in 2 there is the value*)
					datasetToReturn[Select[#[params[[1]]] == ToExpression[params[[2]]] &]]
					), 
					datasetToReturn];
				datasetToReturn = If[
					MatchQ[queryCharacters, {__, ">" , __}], 
					(params = StringSplit[qReplaced, ">"]; (* In 1 there is the attribute, in 2 there is the value*)
					datasetToReturn[Select[#[params[[1]]] > ToExpression[params[[2]]] &]]
					), 
					datasetToReturn]
				,{q, queries}];
			Return[datasetToReturn]
		]
    
     (* Filters the given dataset by the given last name *)
	FilterLastName[dataset_, lastname_] := If[lastname === Null, dataset, dataset[Select[#COGNOME == lastname &]]]
	
	(* Filters the given dataset by the given first name *)
	FilterFirstName[dataset_, firstname_] := If[firstname === Null, dataset, dataset[Select[#NOME == firstname &]]]
	
     (* Filters the given dataset by the given city *)
	FilterCity[dataset_, city_] := If[city === Null, dataset, dataset[Select[#COMUNE == city &]]]

	Options[PlottingElectionElectorsPie] = {region -> Null, province -> Null, district -> Null, query -> Null};
	PlottingElectionElectorsPie[chamber_, opts : OptionsPattern[]] := 
		Module[{dataset, datasetSelectBy, maleElectors, femaleElectors}, 
			dataset = If[chamber === "camera", chamberDataset, senateDataset];
			datasetSelectBy = dataset[DeleteDuplicatesBy["COMUNE"]];
			
			datasetSelectBy = FilterRegion[datasetSelectBy, OptionValue[region]];
			datasetSelectBy = FilterProvince[datasetSelectBy, OptionValue[province]];
			datasetSelectBy = FilterDistrict[datasetSelectBy, OptionValue[district]];
			datasetSelectBy = FilterQuery[datasetSelectBy, OptionValue[query]];
			
			maleElectors = Total[datasetSelectBy[All, "ELETTORIMAS"]];
			femaleElectors = Total[datasetSelectBy[All, "ELETTORIFEM"]];
			Return[{maleElectors, femaleElectors}]
		]
	
	Options[PlottingElectionVotersPie] = {region -> Null, province -> Null, district -> Null, query -> Null};
	PlottingElectionVotersPie[chamber_, opts : OptionsPattern[]] :=
		Module[{dataset, datasetSelectBy, maleVoters, femaleVoters}, 
			dataset = If[chamber === "camera", chamberDataset, senateDataset];
			datasetSelectBy = dataset[DeleteDuplicatesBy["COMUNE"]];
			
			datasetSelectBy = FilterRegion[datasetSelectBy, OptionValue[region]];
			datasetSelectBy = FilterProvince[datasetSelectBy, OptionValue[province]];
			datasetSelectBy = FilterDistrict[datasetSelectBy, OptionValue[district]];
			datasetSelectBy = FilterQuery[datasetSelectBy, OptionValue[query]];
			
			maleVoters = Total[datasetSelectBy[All, "VOTANTIMAS"]];
			femaleVoters = Total[datasetSelectBy[All, "VOTANTIFEM"]];
			Return[{maleVoters, femaleVoters}]
		]
		
	Options[PlottingElectionVotersNonVotersPie] = {region -> Null, province -> Null, district -> Null, query -> Null};
	PlottingElectionVotersNonVotersPie[chamber_, opts : OptionsPattern[]] :=
		Module[{dataset, datasetSelectBy, maleVoters, femaleVoters, maleElectors, femaleElectors}, 
			dataset = If[chamber === "camera", chamberDataset, senateDataset];
			datasetSelectBy = dataset[DeleteDuplicatesBy["COMUNE"]];
			
			datasetSelectBy = FilterRegion[datasetSelectBy, OptionValue[region]];
			datasetSelectBy = FilterProvince[datasetSelectBy, OptionValue[province]];
			datasetSelectBy = FilterDistrict[datasetSelectBy, OptionValue[district]];
			datasetSelectBy = FilterQuery[datasetSelectBy, OptionValue[query]];
			
			maleElectors = Total[datasetSelectBy[All, "ELETTORIMAS"]];
			femaleElectors = Total[datasetSelectBy[All, "ELETTORIFEM"]];
			maleVoters = Total[datasetSelectBy[All, "VOTANTIMAS"]];
			femaleVoters = Total[datasetSelectBy[All, "VOTANTIFEM"]];
			
			Return[{maleVoters, femaleVoters, maleElectors-maleVoters, femaleElectors-femaleVoters}]
		]
		
	Options[PlottingElectionRegionCoalitionsBars] = {region -> Null, province -> Null, district -> Null, query -> Null};
	PlottingElectionRegionCoalitionsBars[chamber_, coalition_, opts : OptionsPattern[]] :=
		Module[{dataset, parties, datasetSelectBy}, 
			dataset = If[chamber === "camera", chamberDataset, senateDataset];
			parties = If[chamber === "camera", coalitionsChamber[[coalition]], coalitionsSenate[[coalition]]];
			datasetSelectBy = dataset;			
			
			datasetSelectBy = FilterRegion[datasetSelectBy, OptionValue[region]];
			datasetSelectBy = FilterProvince[datasetSelectBy, OptionValue[province]];
			datasetSelectBy = FilterDistrict[datasetSelectBy, OptionValue[district]];
			datasetSelectBy = FilterQuery[datasetSelectBy, OptionValue[query]];
			
			(*
			Do[
				regionVotes = datasetSelectBy[Select[GetRegionFromDistrict(#CIRCOSCRIZIONE) \[Equal] region&]];
				partiesVotes = regionVotes[Select[MemberQ[parties, #LISTA] &]];
				AppendTo[Total[partiesVotes[All, "VOTICANDUNINOM"]], coalitionVotesForRegion], 
			{region, regions}];
			*)
			
			Table[Total[datasetSelectBy[Select[GetRegionFromDistrict[#CIRCOSCRIZIONE] == r&]][Select[MemberQ[parties, #LISTA] &]][All, "VOTICANDUNINOM"]], {r, regions}]
		]
		
	Options[PlottingCandidate] = {city -> Null};
	PlottingCandidate[name_, surname_, opts : OptionsPattern[]] :=
	     Module[{chamberDatasetSelectBy, senateDatasetSelectBy, returnDataset, uninominaleName},
	         (* Filtering data for the Chamber of Deputies *)
	         chamberDatasetSelectBy = chamberDataset;
	         
	         chamberDatasetSelectBy = FilterLastName[chamberDatasetSelectBy, surname];
	         chamberDatasetSelectBy = FilterFirstName[chamberDatasetSelectBy, name];
	         chamberDatasetSelectBy = FilterCity[chamberDatasetSelectBy, OptionValue[city]];
	         
	         (* Filtering data for the Senate *)
	         senateDatasetSelectBy = senateDataset;
	         
	         senateDatasetSelectBy = FilterLastName[senateDatasetSelectBy, surname];
	         senateDatasetSelectBy = FilterFirstName[senateDatasetSelectBy, name];
	         senateDatasetSelectBy = FilterCity[senateDatasetSelectBy, OptionValue[city]];
	         
	         If[
	             Length[chamberDatasetSelectBy] > 0, [
	                 (* It is sufficient to get the name of the uninominale from the first row since a candidate can only be present in one uninomale. *)
	                 uninominaleName = chamberDatasetSelectBy[1, "UNINOMINALE"];
	                 returnDataset = chamberDataset;
	                 returnDataset = returnDataset[Select[#UNINOMINALE == uninominaleName &];
	                 returnDataset[ReverseSortBy["VOTISOLOCANDUNINOM"]]
	                 Return[{returnDataset[All, "COGNOME"], returnDataset[All, "NOME"], returnDataset[All, "VOTISOLOCANDUNINOM"]}]
	             ], If[ (* Either a candidate is present in the Chamber or in the Senate or in none of the two. *)
	                 Length[senateDatasetSelectBy] > 0, [
	                     uninominaleName = senateDatasetSelectBy[1, "UNINOMINALE"];
	                     returnDataset = senateDataset;
	                     returnDataset = returnDataset[Select[#UNINOMINALE == uninominaleName &];
	                     returnDataset[ReverseSortBy["VOTISOLOCANDUNINOM"]]
	                     Return[{returnDataset[All, "COGNOME"], returnDataset[All, "NOME"], returnDataset[All, "VOTISOLOCANDUNINOM"]}]
	                 ], []
	             ]
	         ];
	     ]
End[]
EndPackage[]



